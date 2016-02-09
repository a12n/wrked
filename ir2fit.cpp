#include <ctime>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include <fit_encode.hpp>
#include <fit_file_id_mesg.hpp>
#include <fit_workout_mesg.hpp>
#include <fit_workout_step_mesg.hpp>

using std::exception;
using std::istream;
using std::pair;
using std::runtime_error;
using std::string;
using std::vector;

namespace {

//----------------------------------------------------------------------------
// Exceptions

#define DEF_EXCEPTION(_name, _defMesg)                  \
    struct _name : public runtime_error                 \
    {                                                   \
        explicit                                        \
        _name(const string& mesg = (_defMesg)) :        \
            runtime_error(mesg)                         \
        {                                               \
        }                                               \
    }

DEF_EXCEPTION(bad_syntax, "Syntax error");
DEF_EXCEPTION(encode_failed, "FIT encoder failed");
DEF_EXCEPTION(end_of_file, "End of file");
DEF_EXCEPTION(input_failed, "I/O error");

#undef DEF_EXCEPTION

//----------------------------------------------------------------------------
// Input functions

template <class T>
T
value(istream& input)
{
    T ans;
    input >> ans;
    if (input.bad()) throw input_failed();
    if (input.eof()) throw end_of_file();
    if (input.fail()) throw bad_syntax();
    return ans;
}

template <class T>
T
value(istream& input, const T& min, const T& max)
{
    const T ans = value<T>(input);
    if (ans < min || ans > max) {
        throw bad_syntax();
    }
    return ans;
}

template <class T>
T
value(istream& input, const vector<pair<string, T> >& table)
{
    const string token = value<string>(input);
    for (const pair<string, T>& p : table) {
        if (token == p.first) return p.second;
    }
    throw bad_syntax();
}

template <class T>
void
match(istream& input, const T& pattern)
{
    if (pattern != value<T>(input)) {
        throw bad_syntax();
    }
}

#define FOR_EACH_TOKEN(_token, _input)                          \
    for (string _token; _token = value<string>(_input), true;)

//----------------------------------------------------------------------------
// value<T> specializations

template <>
FIT_INTENSITY
value<FIT_INTENSITY>(istream& input)
{
    return value<FIT_INTENSITY>(input, {
            {"active"   , FIT_INTENSITY_ACTIVE   },
            {"rest"     , FIT_INTENSITY_REST     },
            {"warmup"   , FIT_INTENSITY_WARMUP   },
            {"cooldown" , FIT_INTENSITY_COOLDOWN }
        });
}

template <>
FIT_SPORT
value<FIT_SPORT>(istream& input)
{
    return value<FIT_SPORT>(input, {
            {"cycling", FIT_SPORT_CYCLING}
        });
}

template <>
fit::FileIdMesg
value<fit::FileIdMesg>(istream& input)
{
    fit::FileIdMesg ans;

    ans.SetType(FIT_FILE_WORKOUT);
    ans.SetManufacturer(FIT_MANUFACTURER_GARMIN);
    ans.SetProduct(FIT_GARMIN_PRODUCT_EDGE500);
    ans.SetSerialNumber(54321);
    ans.SetTimeCreated(0);

    match<string>(input, "file_id");
    FOR_EACH_TOKEN(token, input) {
        if (token == "serial_number") {
            ans.SetSerialNumber(value<FIT_UINT32Z>(input));
        } else if (token == "time_created") {
            ans.SetTimeCreated(value<FIT_DATE_TIME>(input));
        } else if (token == "end_of_file") {
            break;
        } else {
            throw bad_syntax();
        }
    }

    return ans;
}

template <>
fit::WorkoutMesg
value<fit::WorkoutMesg>(istream& input)
{
    fit::WorkoutMesg ans;

    ans.SetNumValidSteps(1);

    match<string>(input, "workout");
    FOR_EACH_TOKEN(token, input) {
        if (token == "sport") {
            ans.SetSport(value<FIT_SPORT>(input));
        } else if (token == "num_valid_steps") {
            ans.SetNumValidSteps(value<FIT_UINT16>(input, 1, 10000));
        } else if (token == "name") {
            ans.SetWktName(value<FIT_WSTRING>(input));
        } else if (token == "end_workout") {
            break;
        } else {
            throw bad_syntax();
        }
    }

    return ans;
}

template <>
fit::WorkoutStepMesg
value<fit::WorkoutStepMesg>(istream& input)
{
    fit::WorkoutStepMesg ans;

    match<string>(input, "step");
    FOR_EACH_TOKEN(token, input) {
        // TODO
        if (token == "intensity") {
            ans.SetIntensity(value<FIT_INTENSITY>(input));
        } else if (token == "end_step") {
            break;
        } else {
            throw bad_syntax();
        }
    }

    return ans;
}

//----------------------------------------------------------------------------
// Main

void
ir2fit(std::istream& input, std::iostream& output)
{
    fit::Encode encode;
    encode.Open(output);
    encode.Write(value<fit::FileIdMesg>(input));
    const fit::WorkoutMesg workout = value<fit::WorkoutMesg>(input);
    const size_t n = workout.GetNumValidSteps();
    encode.Write(workout);
    for (size_t i = 0; i < n; ++i) {
        encode.Write(value<fit::WorkoutStepMesg>(input));
    }
    if (!encode.Close()) {
        throw encode_failed();
    }
}

} // namespace

#ifndef _WITH_TESTS

int
main()
{
    std::stringstream output(std::ios::out | std::ios::binary);

    try {
        ir2fit(std::cin, output);
    } catch (const exception& exn) {
        std::cerr << exn.what() << std::endl;
        return 1;
    }

    std::cout << output.str();

    return 0;
}

#else  // _WITH_TESTS

#define CATCH_CONFIG_MAIN
#include "catch.hpp"

TEST_CASE("EOF on empty input", "[readLine]")
{
    std::istringstream input;
    CHECK_THROWS_AS(readLine(input), end_of_file);
}

TEST_CASE("Read an incomplete line", "[readLine]")
{
    std::istringstream input("a");
    CHECK(readLine(input) == "a");
    CHECK_THROWS_AS(readLine(input), end_of_file);
}

TEST_CASE("Read full line", "[readLine]")
{
    std::istringstream input("abc\n");
    CHECK(readLine(input) == "abc");
    CHECK_THROWS_AS(readLine(input), end_of_file);
}

TEST_CASE("Read multiple lines", "[readLine]")
{
    std::istringstream input("abc\ndef\n");
    CHECK(readLine(input) == "abc");
    CHECK(readLine(input) == "def");
    CHECK_THROWS_AS(readLine(input), end_of_file);
}

TEST_CASE("Empty input", "[ir2fit]")
{
    std::istringstream input;
    std::stringstream output;
    CHECK_THROWS_AS(ir2fit(input, output), bad_syntax);
}

TEST_CASE("Empty steps list", "[ir2fit]")
{
    std::istringstream input(
        "begin_workout\n"
        "end_workout\n"
        );
    std::stringstream output;
    CHECK_THROWS_AS(ir2fit(input, output), bad_syntax);
}

TEST_CASE("Invalid number of steps", "[ir2fit]")
{
    std::istringstream input(
        "begin_workout\n"
        "num_steps 2\n"
        "begin_step\n"
        "end_step\n"
        "end_workout\n"
        );
    std::stringstream output;
    CHECK_THROWS_AS(ir2fit(input, output), bad_syntax);
}

TEST_CASE("Fails on trailing garbage", "[ir2fit]")
{
    std::istringstream input(
        "begin_workout\n"
        "num_steps 1\n"
        "begin_step\n"
        "end_step\n"
        "end_workout\n"
        "xyz\n"
        );
    std::stringstream output;
    CHECK_THROWS_AS(ir2fit(input, output), bad_syntax);
}

TEST_CASE("Sport and name properties", "[ir2fit]")
{
    std::istringstream input(
        "begin_workout\n"
        "name foo bar\n"
        "sport cycling\n"
        "num_steps 1\n"
        "begin_step\n"
        "end_step\n"
        "end_workout\n"
        );
    std::stringstream output;
    CHECK_NOTHROW(ir2fit(input, output));
}

#endif  // _WITH_TESTS
