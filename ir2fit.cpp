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
using std::istringstream;
using std::ostringstream;
using std::pair;
using std::runtime_error;
using std::string;
using std::stringstream;
using std::vector;

namespace {

#define STRING_STREAM(_expr)                                            \
    static_cast<ostringstream&>(                                        \
        ostringstream().flush() << _expr).str()

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

string
line(istream& input)
{
    string ans;
    getline(input, ans, '\n');
    if (input.bad()) throw input_failed();
    if (input.eof()) throw end_of_file();
    return ans;
}

template <class T>
T
value(istream& input)
{
    T ans;
    istringstream iss(line(input));
    iss >> ans;
    if (iss.fail()) {
        throw bad_syntax(
            STRING_STREAM("Syntax error near \"" << iss.str() << "\""));
    }
    return ans;
}

template <>
string
value<string>(istream& input)
{
    return line(input);
}

template <class T>
void
match(istream& input, const T& pattern)
{
    const T val = value<T>(input);
    if (pattern != val) {
        throw bad_syntax(
            STRING_STREAM("Expected \"" << pattern << "\", "
                          "got \"" << val << "\""));
    }
}

template <class T>
T
value(istream& input, const T& min, const T& max)
{
    const T ans = value<T>(input);
    if (ans < min || ans > max) {
        throw bad_syntax(
            STRING_STREAM("Value \"" << ans << "\" is "
                          "out of range [" << min << ", " << max << "]"));
    }
    return ans;
}

template <class T>
T
value(istream& input, const vector<pair<string, T> >& map)
{
    const string val = value<string>(input);
    for (const pair<string, T>& p : map) {
        if (p.first == val) return p.second;
    }
    throw bad_syntax(
        STRING_STREAM("Invalid enum value \"" << val << "\""));
}

#define FOR_EACH_TOKEN(_token, _input)                          \
    for (string _token; _token = value<string>(_input), true;)

//----------------------------------------------------------------------------
// value<T> specializations

FIT_WKT_STEP_DURATION
step_duration_value(istream& input)
{
    return value<FIT_WKT_STEP_DURATION>(input, {
            {"time"                            , FIT_WKT_STEP_DURATION_TIME                            },
            {"distance"                        , FIT_WKT_STEP_DURATION_DISTANCE                        },
            {"hr_less_than"                    , FIT_WKT_STEP_DURATION_HR_LESS_THAN                    },
            {"hr_greater_than"                 , FIT_WKT_STEP_DURATION_HR_GREATER_THAN                 },
            {"colories"                        , FIT_WKT_STEP_DURATION_CALORIES                        },
            {"open"                            , FIT_WKT_STEP_DURATION_OPEN                            },
            {"repeat_until_steps_cmplt"        , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_STEPS_CMPLT        },
            {"repeat_until_time"               , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_TIME               },
            {"repeat_until_distance"           , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_DISTANCE           },
            {"repeat_until_calories"           , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_CALORIES           },
            {"repeat_until_hr_less_than"       , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_HR_LESS_THAN       },
            {"repeat_until_hr_greater_than"    , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_HR_GREATER_THAN    },
            {"repeat_until_power_less_than"    , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_POWER_LESS_THAN    },
            {"repeat_until_power_greater_than" , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_POWER_GREATER_THAN },
            {"power_less_than"                 , FIT_WKT_STEP_DURATION_POWER_LESS_THAN                 },
            {"power_greater_than"              , FIT_WKT_STEP_DURATION_POWER_GREATER_THAN              },
            {"repetition_time"                 , FIT_WKT_STEP_DURATION_REPETITION_TIME                 }
        });
}

FIT_WKT_STEP_TARGET
step_target_value(istream& input)
{
    return value<FIT_WKT_STEP_TARGET>(input, {
            {"speed"      , FIT_WKT_STEP_TARGET_SPEED      },
            {"heart_rate" , FIT_WKT_STEP_TARGET_HEART_RATE },
            {"open"       , FIT_WKT_STEP_TARGET_OPEN       },
            {"cadence"    , FIT_WKT_STEP_TARGET_CADENCE    },
            {"power"      , FIT_WKT_STEP_TARGET_POWER      },
            {"grade"      , FIT_WKT_STEP_TARGET_GRADE      },
            {"resistance" , FIT_WKT_STEP_TARGET_RESISTANCE }
        });
}

FIT_INTENSITY
intensity_value(istream& input)
{
    return value<FIT_INTENSITY>(input, {
            {"active"   , FIT_INTENSITY_ACTIVE   },
            {"rest"     , FIT_INTENSITY_REST     },
            {"warmup"   , FIT_INTENSITY_WARMUP   },
            {"cooldown" , FIT_INTENSITY_COOLDOWN }
        });
}

FIT_SPORT
sport_value(istream& input)
{
    return value<FIT_SPORT>(input, {
            {"cycling", FIT_SPORT_CYCLING}
        });
}

fit::FileIdMesg
file_id_value(istream& input)
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
            throw bad_syntax(
                STRING_STREAM("Unexpected token \"" << token <<
                              "\" in \"file_id\" message"));
        }
    }

    return ans;
}

fit::WorkoutMesg
workout_value(istream& input)
{
    fit::WorkoutMesg ans;

    ans.SetNumValidSteps(1);

    match<string>(input, "workout");
    FOR_EACH_TOKEN(token, input) {
        if (token == "sport") {
            ans.SetSport(sport_value(input));
        } else if (token == "num_valid_steps") {
            ans.SetNumValidSteps(value<FIT_UINT16>(input, 1, 10000));
        } else if (token == "name") {
            // TODO
            // ans.SetWktName(value<FIT_WSTRING>(input));
        } else if (token == "end_workout") {
            break;
        } else {
            throw bad_syntax(
                STRING_STREAM("Unexpected token \"" << token <<
                              "\" in \"workout\" message"));
        }
    }

    return ans;
}

fit::WorkoutStepMesg
workout_step_value(istream& input)
{
    fit::WorkoutStepMesg ans;

    ans.SetDurationType(FIT_WKT_STEP_DURATION_OPEN);
    ans.SetTargetType(FIT_WKT_STEP_TARGET_OPEN);

    match<string>(input, "step");
    FOR_EACH_TOKEN(token, input) {
        // TODO
        if (token == "intensity") {
            ans.SetIntensity(intensity_value(input));
        } else if (token == "name") {
            // ans.SetWktStepName(value<FIT_WSTRING>(input));
        } else if (token == "duration_type") {
            ans.SetDurationType(step_duration_value(input));
        } else if (token == "duration_value") {
            ans.SetDurationValue(value<FIT_UINT32>(input));
        } else if (token == "duration_time") {
            ans.SetDurationTime(value<FIT_FLOAT32>(input));
        } else if (token == "target_type") {
            ans.SetTargetType(step_target_value(input));
        } else if (token == "end_step") {
            break;
        } else {
            throw bad_syntax(
                STRING_STREAM("Unexpected token \"" << token <<
                              "\" in \"workout_step\" message"));
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
    encode.Write(file_id_value(input));

    const fit::WorkoutMesg workout = workout_value(input);
    const FIT_UINT16 n = workout.GetNumValidSteps();

    encode.Write(workout);

    for (FIT_UINT16 i = 0; i < n; ++i) {
        fit::WorkoutStepMesg workout_step = workout_step_value(input);
        workout_step.SetMessageIndex(i);
        encode.Write(workout_step);
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

TEST_CASE("EOF on empty input", "[line]")
{
    istringstream input;
    CHECK_THROWS_AS(line(input), end_of_file);
}

TEST_CASE("Read an incomplete line", "[line]")
{
    istringstream input("a");
    CHECK(line(input) == "a");
    CHECK_THROWS_AS(line(input), end_of_file);
}

TEST_CASE("Read full line", "[line]")
{
    istringstream input("abc\n");
    CHECK(line(input) == "abc");
    CHECK_THROWS_AS(line(input), end_of_file);
}

TEST_CASE("Read multiple lines", "[line]")
{
    istringstream input("abc\ndef\n");
    CHECK(line(input) == "abc");
    CHECK(line(input) == "def");
    CHECK_THROWS_AS(line(input), end_of_file);
}

TEST_CASE("Empty input", "[ir2fit]")
{
    istringstream input;
    stringstream output;
    CHECK_THROWS_AS(ir2fit(input, output), bad_syntax);
}

#endif  // _WITH_TESTS
