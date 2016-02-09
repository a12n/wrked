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

//----------------------------------------------------------------------------
// State functions

#define DEF_STATE(_name)                                \
    void                                                \
    _name (std::istream& input, fit::Encode& encode)

#define DECL_STATE(_name) DEF_STATE(_name)

#define NEXT_STATE(_name)                       \
    _name(input, encode);                       \
    return

#define STATE_LOOP(_name)                                           \
    for (std::string inputLine; inputLine = readLine(input), true;)

DECL_STATE(start);
DECL_STATE(workout);
DECL_STATE(workoutStep);
DECL_STATE(finish);

DEF_STATE(start)
{
    fit::FileIdMesg mesg;

    mesg.SetType(FIT_FILE_WORKOUT);
    mesg.SetManufacturer(FIT_MANUFACTURER_GARMIN);
    mesg.SetProduct(FIT_GARMIN_PRODUCT_EDGE500);
    mesg.SetSerialNumber(54321);
    mesg.SetTimeCreated(0);

    encode.Write(mesg);

    STATE_LOOP() {
        if (inputLine == "begin_workout") {
            NEXT_STATE(workout);
        } else {
            throw BadSyntax();
        }
    }
}

DEF_STATE(workout)
{
    fit::WorkoutMesg mesg;

    // TODO

    STATE_LOOP() {
        if (inputLine == "begin_step") {
            // TODO
            NEXT_STATE(workoutStep);
        } else if (inputLine == "end_workout") {
            // TODO
            NEXT_STATE(finish);
        } else {
            throw BadSyntax();
        }
        // TODO
    }
}

DEF_STATE(workoutStep)
{
    fit::WorkoutStepMesg mesg;

    // TODO

    STATE_LOOP() {
        if (inputLine == "end_step") {
            // TODO
            NEXT_STATE(workout);
        } else {
            throw BadSyntax();
        }
        // TODO
    }
}

DEF_STATE(finish)
{
    try {
        STATE_LOOP() {
            throw BadSyntax();
        }
    } catch (const EndOfFile&) {
        if (!encode.Close()) {
            throw EncodeFailed();
        }
    }
}

#undef NEXT_STATE
#undef DECL_STATE
#undef DEF_STATE

//----------------------------------------------------------------------------
// Main

void
ir2fit(std::istream& input, std::iostream& output)
{
    fit::Encode encode;

    encode.Open(output);

    start(input, encode);
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
