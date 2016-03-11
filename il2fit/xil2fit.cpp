#include <ctime>
#include <experimental/optional>
#include <functional>
#include <iostream>
#include <iterator>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <fit_date_time.hpp>
#include <fit_encode.hpp>
#include <fit_file_creator_mesg.hpp>
#include <fit_file_id_mesg.hpp>
#include <fit_workout_mesg.hpp>
#include <fit_workout_step_mesg.hpp>

using std::cerr;
using std::cout;
using std::experimental::bad_optional_access;
using std::experimental::nullopt;
using std::experimental::optional;
using std::function;
using std::get;
using std::getline;
using std::istream;
using std::istringstream;
using std::make_pair;
using std::ostringstream;
using std::out_of_range;
using std::pair;
using std::runtime_error;
using std::string;
using std::unordered_map;
using std::vector;

#define S(_expr)                                                   \
    static_cast<ostringstream&>(                                   \
        ostringstream().flush() << _expr).str()

namespace {

void
error(const string& descr = "")
{
    throw runtime_error(descr);
}

//----------------------------------------------------------------------------
// Optional

const auto none = nullopt;

template <class T>
optional<T>
some(const T& v)
{
    return optional<T>(v);
}

//----------------------------------------------------------------------------
// Line-based input

optional<string>
line(istream& input)
{
    string ans;
    getline(input, ans, '\n');
    if (input.bad()) {
        error("I/O error");
    }
    if (input.eof() && ans.empty()) {
        return none;
    }
    return some(ans);
}

//----------------------------------------------------------------------------
// Parse value from input

template <class T>
T
value(istream& input);

template <>
string
value<string>(istream& input)
{
    try {
        return line(input).value();
    } catch (const bad_optional_access&) {
        error("Unexpected end of file");
    }
}

template <class T>
T
value(istream& input)
{
    const auto k = value<string>(input);
    T ans;
    istringstream iss(k);
    iss >> ans;
    if (iss.fail()) {
        error("Bad syntax near \"" + k + "\"");
    }
    return ans;
}

//----------------------------------------------------------------------------
// Call actions on input tokens

void
match(const string& token,
      const unordered_map<string, function<void()> >& actions)
{
    try {
        actions.at(token)();
    } catch (const out_of_range&) {
        error("Bad token \"" + token + "\"");
    }
}

void
match(istream& input,
      const unordered_map<string, function<void()> >& actions)
{
    match(value<string>(input), actions);
}

//----------------------------------------------------------------------------
// Parse FIT messages from input

template <>
fit::FileCreatorMesg
value<fit::FileCreatorMesg>(istream& input)
{
    fit::FileCreatorMesg ans;

    for (bool done = false; !done;) {
        match(input, {
                { "hardware_version", [&] {
                        ans.SetHardwareVersion(value<FIT_UINT8>(input)); }},
                { "software_version", [&] {
                        ans.SetSoftwareVersion(value<FIT_UINT16>(input)); }},
                { "end", [&] {
                        match(input, {
                                { "file_creator", [&] { done = true; }} });
                    }}
            });
    }

    return ans;
}

template <>
fit::FileIdMesg
value<fit::FileIdMesg>(istream& input)
{
    fit::FileIdMesg ans;

    // Default values
    ans.SetType(FIT_FILE_WORKOUT);
    ans.SetManufacturer(FIT_MANUFACTURER_GARMIN);
    ans.SetProduct(FIT_GARMIN_PRODUCT_EDGE500);
    ans.SetSerialNumber(54321);
    ans.SetTimeCreated(fit::DateTime(time(0)).GetTimeStamp());

    while (true) {
        const auto k = value<string>(input);
        // "number";
        // "serial_number";
        // "time_created";
        // 
        // "end";
    }

    return ans;
}

template <>
fit::WorkoutMesg
value<fit::WorkoutMesg>(istream& input)
{
    fit::WorkoutMesg ans;
    // TODO
    return ans;
}

template <>
fit::WorkoutStepMesg
value<fit::WorkoutStepMesg>(istream& input)
{
    fit::WorkoutStepMesg ans;
    // TODO
    return ans;
}

void
xil2fit(std::istream& input, std::iostream& output)
{
    fit::Encode encode;

    encode.Open(output);

    while (const auto lopt = line(input)) {
        match(lopt.value(), {
                { "begin", [&] {
                        match(input, {
                                { "file_creator", [&] {
                                        encode.Write(value<fit::FileCreatorMesg>(input)); }},
                                { "file_id", [&] {
                                        encode.Write(value<fit::FileIdMesg>(input)); }},
                                { "workout", [&] {
                                        encode.Write(value<fit::WorkoutMesg>(input)); }},
                                { "workout_step", [&] {
                                        encode.Write(value<fit::WorkoutStepMesg>(input)); }}
                            });
                    }}
            });
    }

    if (!encode.Close()) {
        error("FIT encoder failed");
    }
}

} // namespace

//----------------------------------------------------------------------------
// Main

#ifndef _WITH_TESTS

int main()
{
    std::stringstream output(std::ios::out | std::ios::binary);

    try {
        xil2fit(std::cin, output);
    } catch (const std::exception& exn) {
        std::cerr << exn.what() << std::endl;
        return 1;
    }

    std::cout << output.str();

    return 0;
}

#else  // _WITH_TESTS

#define CATCH_CONFIG_MAIN
#include <catch.hpp>

//----------------------------------------------------------------------------
// Cases for line()

TEST_CASE("EOF on empty input", "[line]")
{
    istringstream input;
    CHECK(line(input) == none);
}

TEST_CASE("Empty line", "[line]")
{
    istringstream input("\n");
    CHECK(line(input) == some(string("")));
    CHECK(line(input) == none);
}

TEST_CASE("Incomplete line", "[line]")
{
    istringstream input("abc");
    CHECK(line(input) == some(string("abc")));
    CHECK(line(input) == none);
}

TEST_CASE("Single complete line", "[line]")
{
    istringstream input("abc\n");
    CHECK(line(input) == some(string("abc")));
    CHECK(line(input) == none);
}

TEST_CASE("Multiple lines", "[line]")
{
    istringstream input("abc\ndef\n");
    CHECK(line(input) == some(string("abc")));
    CHECK(line(input) == some(string("def")));
    CHECK(line(input) == none);
}

//----------------------------------------------------------------------------
// Cases for value()

TEST_CASE("Error on empty input", "[value]")
{
    istringstream input;
    CHECK_THROWS_AS(value<string>(input), runtime_error);
}

TEST_CASE("Parse string value", "[value]")
{
    istringstream input("abc\n");
    CHECK(value<string>(input) == "abc");
}

TEST_CASE("Parse int value", "[value]")
{
    istringstream input("123\n");
    CHECK(value<int>(input) == 123);
}

TEST_CASE("Parse double value", "[value]")
{
    istringstream input("45.6\n");
    CHECK(value<double>(input) == 45.6);
}

//----------------------------------------------------------------------------
// Cases for value() with range

TEST_CASE("Parse value with range", "[value]")
{
    istringstream input("55\n");
    CHECK(value<int>(input, 0, 100) == 55);
}

#endif  // _WITH_TESTS
