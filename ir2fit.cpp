#include <ctime>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

#include <fit_encode.hpp>
#include <fit_file_id_mesg.hpp>
#include <fit_workout_mesg.hpp>
#include <fit_workout_step_mesg.hpp>

namespace {

//----------------------------------------------------------------------------
// Exceptions

#define DEF_EXCEPTION(_name, _descr)            \
    struct _name : public std::exception        \
    {                                           \
        virtual const char*                     \
        what() const noexcept                   \
        {                                       \
            return _descr;                      \
        }                                       \
    }

DEF_EXCEPTION(BadSyntax, "Syntax error");
DEF_EXCEPTION(EncodeFailed, "FIT encoder failed");
DEF_EXCEPTION(EndOfFile, "End of file");
DEF_EXCEPTION(InputFailed, "I/O error");

#undef DEF_EXCEPTION

//----------------------------------------------------------------------------
// Input functions

std::string
readLine(std::istream& input)
{
    std::string line;
    if (!std::getline(input, line, '\n')) {
        if (input.eof()) {
            throw EndOfFile();
        } else {
            throw InputFailed();
        }
    }
    return line;
}

template <class T>
T
value(std::istream& input)
{
    T ans;
    input >> ans;
    if (input.bad()) throw InputFailed();
    if (input.eof()) throw EndOfFile();
    if (input.fail()) throw BadSyntax();
    return ans;
}

template <class T>
void
match(std::istream& input, const T& pattern)
{
    if (pattern != value<T>(input)) {
        throw BadSyntax();
    }
}

//----------------------------------------------------------------------------
// Readers

fit::FileIdMesg
fileId(std::istream& input)
{
    fit::FileIdMesg ans;
    // TODO: read manufacturer, product, serial number, time created?
    ans.SetType(FIT_FILE_WORKOUT);
    ans.SetManufacturer(FIT_MANUFACTURER_GARMIN);
    ans.SetProduct(FIT_GARMIN_PRODUCT_EDGE500);
    ans.SetSerialNumber(54321);
    ans.SetTimeCreated(0);
    return ans;
}

fit::WorkoutMesg
workout(std::istream& input, size_t& nSteps)
{
    fit::WorkoutMesg ans;
    // TODO
    nSteps = 0;
    return ans;
}

fit::WorkoutStepMesg
workoutStep(std::istream& input)
{
    fit::WorkoutStepMesg ans;
    // TODO
    return ans;
}

//----------------------------------------------------------------------------
// Main

void
ir2fit(std::istream& input, std::iostream& output)
{
    fit::Encode encode;
    encode.Open(output);
    encode.Write(fileId(input));
    size_t nSteps = 0;
    encode.Write(workout(input, nSteps));
    for (size_t i = 0; i < nSteps; ++i) {
        encode.Write(workoutStep(input));
    }
    if (!encode.Close()) {
        throw EncodeFailed();
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
    } catch (const std::exception& exn) {
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
    CHECK_THROWS_AS(readLine(input), EndOfFile);
}

TEST_CASE("Read an incomplete line", "[readLine]")
{
    std::istringstream input("a");
    CHECK(readLine(input) == "a");
    CHECK_THROWS_AS(readLine(input), EndOfFile);
}

TEST_CASE("Read full line", "[readLine]")
{
    std::istringstream input("abc\n");
    CHECK(readLine(input) == "abc");
    CHECK_THROWS_AS(readLine(input), EndOfFile);
}

TEST_CASE("Read multiple lines", "[readLine]")
{
    std::istringstream input("abc\ndef\n");
    CHECK(readLine(input) == "abc");
    CHECK(readLine(input) == "def");
    CHECK_THROWS_AS(readLine(input), EndOfFile);
}

TEST_CASE("Empty input", "[ir2fit]")
{
    std::istringstream input;
    std::stringstream output;
    CHECK_THROWS_AS(ir2fit(input, output), BadSyntax);
}

TEST_CASE("Empty steps list", "[ir2fit]")
{
    std::istringstream input(
        "begin_workout\n"
        "end_workout\n"
        );
    std::stringstream output;
    CHECK_THROWS_AS(ir2fit(input, output), BadSyntax);
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
    CHECK_THROWS_AS(ir2fit(input, output), BadSyntax);
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
    CHECK_THROWS_AS(ir2fit(input, output), BadSyntax);
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
