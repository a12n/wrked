#include <algorithm>
#include <experimental/optional>
#include <functional>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wignored-qualifiers"

#include <fit_date_time.hpp>
#include <fit_encode.hpp>
#include <fit_file_creator_mesg.hpp>
#include <fit_file_id_mesg.hpp>
#include <fit_workout_mesg.hpp>
#include <fit_workout_step_mesg.hpp>

#pragma GCC diagnostic pop

using std::cerr;
using std::cin;
using std::cout;
using std::endl;
using std::exception;
using std::experimental::bad_optional_access;
using std::experimental::nullopt;
using std::experimental::optional;
using std::function;
using std::getline;
using std::ios;
using std::iostream;
using std::istream;
using std::istringstream;
using std::make_pair;
using std::max;
using std::min;
using std::ostringstream;
using std::out_of_range;
using std::pair;
using std::runtime_error;
using std::string;
using std::stringstream;
using std::unordered_map;
using std::wstring;

#define S(_expr)                                                   \
    static_cast<ostringstream&>(                                   \
        ostringstream().flush() << _expr).str()

namespace {

[[noreturn]]
void
error(const string& descr = "")
{
    throw runtime_error(descr);
}

string
trim(const string& s)
{
    string ans = s;
    // Left
    const auto l = find_if_not(ans.begin(), ans.end(), isspace);
    ans.erase(ans.begin(), l);
    // Right
    const auto r = find_if_not(ans.rbegin(), ans.rend(), isspace).base();
    ans.erase(r, ans.end());
    return ans;
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
        return trim(line(input).value());
    } catch (const bad_optional_access&) {
        error("Unexpected end of file");
    }
}

template <>
wstring
value<wstring>(istream& input)
{
    // FIXME: Invalid conversion
    const auto s = value<string>(input);
    return wstring(s.begin(), s.end());
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
// Parse value from input and check range

template <class T>
T
value(istream& input, const T& a, const T& b)
{
    const auto p = min(a, b);
    const auto q = max(a, b);
    const T ans = value<T>(input);
    if (ans < p || ans > q) {
        error(S("Value " << ans << " is out of range "
                "[" << p << ", " << q << "]"));
    }
    return ans;
}

template <class T>
T
value(istream& input, const pair<T, T>& range)
{
    return value<T>(input, range.first, range.second);
}

//----------------------------------------------------------------------------
// Parse enum value from input

template <class T>
T
value(istream& input, const unordered_map<string, T>& table)
{
    const auto token = value<string>(input);
    try {
        return table.at(token);
    } catch (const out_of_range&) {
        error("Invalid enum value \"" + token + "\"");
    }
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

    for (bool done = false; !done;) {
        match(input, {
                { "number", [&] {
                        ans.SetNumber(value<FIT_UINT16>(input)); } },
                { "serial_number", [&] {
                        ans.SetSerialNumber(value<FIT_UINT32Z>(input)); } },
                { "time_created", [&] {
                        ans.SetTimeCreated(value<FIT_DATE_TIME>(input)); } },
                { "end", [&] {
                        match(input, {
                                { "file_id", [&] { done = true; }} });
                    }}
            });
    }

    return ans;
}

template <>
fit::WorkoutMesg
value<fit::WorkoutMesg>(istream& input)
{
    static const unordered_map<string, FIT_SPORT> sports = {
        { "generic"                 , FIT_SPORT_GENERIC                 },
        { "running"                 , FIT_SPORT_RUNNING                 },
        { "cycling"                 , FIT_SPORT_CYCLING                 },
        { "transition"              , FIT_SPORT_TRANSITION              },
        { "fitness_equipment"       , FIT_SPORT_FITNESS_EQUIPMENT       },
        { "swimming"                , FIT_SPORT_SWIMMING                },
        { "basketball"              , FIT_SPORT_BASKETBALL              },
        { "soccer"                  , FIT_SPORT_SOCCER                  },
        { "tennis"                  , FIT_SPORT_TENNIS                  },
        { "american_football"       , FIT_SPORT_AMERICAN_FOOTBALL       },
        { "training"                , FIT_SPORT_TRAINING                },
        { "walking"                 , FIT_SPORT_WALKING                 },
        { "cross_country_skiing"    , FIT_SPORT_CROSS_COUNTRY_SKIING    },
        { "alpine_skiing"           , FIT_SPORT_ALPINE_SKIING           },
        { "snowboarding"            , FIT_SPORT_SNOWBOARDING            },
        { "rowing"                  , FIT_SPORT_ROWING                  },
        { "mountaineering"          , FIT_SPORT_MOUNTAINEERING          },
        { "hiking"                  , FIT_SPORT_HIKING                  },
        { "multisport"              , FIT_SPORT_MULTISPORT              },
        { "paddling"                , FIT_SPORT_PADDLING                },
        { "flying"                  , FIT_SPORT_FLYING                  },
        { "e_biking"                , FIT_SPORT_E_BIKING                },
        { "motorcycling"            , FIT_SPORT_MOTORCYCLING            },
        { "boating"                 , FIT_SPORT_BOATING                 },
        { "driving"                 , FIT_SPORT_DRIVING                 },
        { "golf"                    , FIT_SPORT_GOLF                    },
        { "hang_gliding"            , FIT_SPORT_HANG_GLIDING            },
        { "horseback_riding"        , FIT_SPORT_HORSEBACK_RIDING        },
        { "hunting"                 , FIT_SPORT_HUNTING                 },
        { "fishing"                 , FIT_SPORT_FISHING                 },
        { "inline_skating"          , FIT_SPORT_INLINE_SKATING          },
        { "rock_climbing"           , FIT_SPORT_ROCK_CLIMBING           },
        { "sailing"                 , FIT_SPORT_SAILING                 },
        { "ice_skating"             , FIT_SPORT_ICE_SKATING             },
        { "sky_diving"              , FIT_SPORT_SKY_DIVING              },
        { "snowshoeing"             , FIT_SPORT_SNOWSHOEING             },
        { "snowmobiling"            , FIT_SPORT_SNOWMOBILING            },
        { "stand_up_paddleboarding" , FIT_SPORT_STAND_UP_PADDLEBOARDING },
        { "surfing"                 , FIT_SPORT_SURFING                 },
        { "wakeboarding"            , FIT_SPORT_WAKEBOARDING            },
        { "water_skiing"            , FIT_SPORT_WATER_SKIING            },
        { "kayaking"                , FIT_SPORT_KAYAKING                },
        { "rafting"                 , FIT_SPORT_RAFTING                 },
        { "windsurfing"             , FIT_SPORT_WINDSURFING             },
        { "kitesurfing"             , FIT_SPORT_KITESURFING             }
    };

    fit::WorkoutMesg ans;

    // Default values
    ans.SetSport(FIT_SPORT_CYCLING);
    ans.SetCapabilities(FIT_WORKOUT_CAPABILITIES_INVALID);
    ans.SetNumValidSteps(1);

    for (bool done = false; !done;) {
        match(input, {
                { "capabilities", [&] {
                        ans.SetCapabilities(
                            value<FIT_WORKOUT_CAPABILITIES>(input)); } },
                { "num_valid_steps", [&] {
                        ans.SetNumValidSteps(
                            value<FIT_UINT16>(input, 1, 10000)); } },
                { "sport", [&] {
                        ans.SetSport(value<FIT_SPORT>(input, sports)); } },
                { "wkt_name", [&] {
                        ans.SetWktName(value<FIT_WSTRING>(input)); } },
                { "end", [&] {
                        match(input, {
                                { "workout", [&] { done = true; }} });
                    }}
            });
    }

    return ans;
}

template <>
fit::WorkoutStepMesg
value<fit::WorkoutStepMesg>(istream& input)
{
    static const unordered_map<string, FIT_INTENSITY> intensities = {
        { "active"   , FIT_INTENSITY_ACTIVE   },
        { "rest"     , FIT_INTENSITY_REST     },
        { "warmup"   , FIT_INTENSITY_WARMUP   },
        { "cooldown" , FIT_INTENSITY_COOLDOWN }
    };

    static const unordered_map<string, FIT_WKT_STEP_DURATION> duration_types = {
        { "time"                            , FIT_WKT_STEP_DURATION_TIME                            },
        { "distance"                        , FIT_WKT_STEP_DURATION_DISTANCE                        },
        { "hr_less_than"                    , FIT_WKT_STEP_DURATION_HR_LESS_THAN                    },
        { "hr_greater_than"                 , FIT_WKT_STEP_DURATION_HR_GREATER_THAN                 },
        { "colories"                        , FIT_WKT_STEP_DURATION_CALORIES                        },
        { "open"                            , FIT_WKT_STEP_DURATION_OPEN                            },
        { "repeat_until_steps_cmplt"        , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_STEPS_CMPLT        },
        { "repeat_until_time"               , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_TIME               },
        { "repeat_until_distance"           , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_DISTANCE           },
        { "repeat_until_calories"           , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_CALORIES           },
        { "repeat_until_hr_less_than"       , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_HR_LESS_THAN       },
        { "repeat_until_hr_greater_than"    , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_HR_GREATER_THAN    },
        { "repeat_until_power_less_than"    , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_POWER_LESS_THAN    },
        { "repeat_until_power_greater_than" , FIT_WKT_STEP_DURATION_REPEAT_UNTIL_POWER_GREATER_THAN },
        { "power_less_than"                 , FIT_WKT_STEP_DURATION_POWER_LESS_THAN                 },
        { "power_greater_than"              , FIT_WKT_STEP_DURATION_POWER_GREATER_THAN              },
        { "repetition_time"                 , FIT_WKT_STEP_DURATION_REPETITION_TIME                 }
    };

    static const unordered_map<string, FIT_WKT_STEP_TARGET> target_types = {
        { "speed"      , FIT_WKT_STEP_TARGET_SPEED      },
        { "heart_rate" , FIT_WKT_STEP_TARGET_HEART_RATE },
        { "open"       , FIT_WKT_STEP_TARGET_OPEN       },
        { "cadence"    , FIT_WKT_STEP_TARGET_CADENCE    },
        { "power"      , FIT_WKT_STEP_TARGET_POWER      },
        { "grade"      , FIT_WKT_STEP_TARGET_GRADE      },
        { "resistance" , FIT_WKT_STEP_TARGET_RESISTANCE }
    };

    static const pair<FIT_WORKOUT_HR, FIT_WORKOUT_HR> hr_range =
        make_pair(0, 355);

    static const pair<FIT_WORKOUT_POWER, FIT_WORKOUT_POWER> power_range =
        make_pair(0, 11000);

    fit::WorkoutStepMesg ans;

    // Default values
    ans.SetMessageIndex(FIT_MESSAGE_INDEX_INVALID);
    ans.SetDurationType(FIT_WKT_STEP_DURATION_OPEN);
    ans.SetTargetType(FIT_WKT_STEP_TARGET_OPEN);

    for (bool done = false; !done;) {
        match(input, {
                { "custom_target_cadence_high", [&] {
                        ans.SetCustomTargetCadenceHigh(
                            value<FIT_UINT32>(input)); // rpm
                    } },
                { "custom_target_cadence_low", [&] {
                        ans.SetCustomTargetCadenceLow(
                            value<FIT_UINT32>(input)); // rpm
                    } },
                { "custom_target_heart_rate_high", [&] {
                        ans.SetCustomTargetHeartRateHigh(
                            value(input, hr_range)); // % or bpm
                    } },
                { "custom_target_heart_rate_low", [&] {
                        ans.SetCustomTargetHeartRateLow(
                            value(input, hr_range)); // % or bpm
                    } },
                { "custom_target_power_high", [&] {
                        ans.SetCustomTargetPowerHigh(
                            value(input, power_range)); // % or W
                    } },
                { "custom_target_power_low", [&] {
                        ans.SetCustomTargetPowerLow(
                            value(input, power_range)); // % or W
                    } },
                { "custom_target_speed_high", [&] {
                        ans.SetCustomTargetSpeedHigh(
                            value<FIT_FLOAT32>(input)); // m/s
                    } },
                { "custom_target_speed_low", [&] {
                        ans.SetCustomTargetSpeedLow(
                            value<FIT_FLOAT32>(input)); // m/s
                    } },
                { "custom_target_value_high", [&] {
                        ans.SetCustomTargetValueHigh(value<FIT_UINT32>(input));
                    } },
                { "custom_target_value_low", [&] {
                        ans.SetCustomTargetValueLow(value<FIT_UINT32>(input));
                    } },
                { "duration_calories", [&] {
                        // TODO: restrict by range?
                        ans.SetDurationCalories(
                            value<FIT_UINT32>(input)); // kcal
                    } },
                { "duration_distance", [&] {
                        // TODO: restrict by range?
                        ans.SetDurationDistance(value<FIT_FLOAT32>(input)); // m
                    } },
                { "duration_hr", [&] {
                        ans.SetDurationHr(value(input, hr_range)); // % or bpm
                    } },
                { "duration_power", [&] {
                        ans.SetDurationPower(
                            value(input, power_range)); // % or W
                    } },
                { "duration_step", [&] {
                        ans.SetDurationStep(value<FIT_UINT32>(input));
                    } },
                { "duration_time", [&] {
                        // TODO: restrict by range?
                        ans.SetDurationTime(value<FIT_FLOAT32>(input)); // s
                    } },
                { "duration_type", [&] {
                        ans.SetDurationType(value(input, duration_types));
                    } },
                { "duration_value", [&] {
                        ans.SetDurationValue(value<FIT_UINT32>(input));
                    } },
                { "intensity", [&] {
                        ans.SetIntensity(value(input, intensities));
                    } },
                { "repeat_calories", [&] {
                        // TODO: restrict by range?
                        ans.SetRepeatCalories(value<FIT_UINT32>(input)); // kcal
                    } },
                { "repeat_distance", [&] {
                        // TODO: restrict by range?
                        ans.SetRepeatDistance(value<FIT_FLOAT32>(input)); // m
                    } },
                { "repeat_hr", [&] {
                        ans.SetRepeatHr(value(input, hr_range)); // % or bpm
                    } },
                { "repeat_power", [&] {
                        ans.SetRepeatPower(value(input, power_range)); // % or W
                    } },
                { "repeat_steps", [&] {
                        ans.SetRepeatSteps(value<FIT_UINT32>(input, 1, 1000));
                    } },
                { "repeat_time", [&] {
                        // TODO: restrict by range?
                        ans.SetRepeatTime(value<FIT_FLOAT32>(input)); // s
                    } },
                { "target_hr_zone", [&] {
                        // HR Zone (1-5); Custom = 0;
                        ans.SetTargetHrZone(value<FIT_UINT32>(input, 0, 5));
                    } },
                { "target_power_zone", [&] {
                        // Power Zone (1-7); Custom = 0;
                        ans.SetTargetPowerZone(value<FIT_UINT32>(input, 0, 7));
                    } },
                { "target_type", [&] {
                        ans.SetTargetType(value(input, target_types));
                    } },
                { "target_value", [&] {
                        ans.SetTargetValue(value<FIT_UINT32>(input));
                    } },
                { "wkt_step_name", [&] {
                        ans.SetWktStepName(value<FIT_WSTRING>(input));
                    } },
                { "end", [&] {
                        match(input, {
                                { "workout_step", [&] { done = true; }} });
                    }}
            });
    }

    return ans;
}

void
xil2fit(istream& input, iostream& output)
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
    stringstream output(ios::out | ios::binary);

    try {
        xil2fit(cin, output);
    } catch (const exception& exn) {
        cerr << exn.what() << endl;
        return 1;
    }

    cout << output.str();

    return 0;
}

#else  // _WITH_TESTS

#define CATCH_CONFIG_MAIN
#include <catch.hpp>

//----------------------------------------------------------------------------
// Cases for trim()

TEST_CASE("Trim empty string", "[trim]")
{
    CHECK(trim("") == "");
}

TEST_CASE("Trim string, empty ans", "[trim]")
{
    CHECK(trim(" \t\t ") == "");
}

TEST_CASE("Trim string left", "[trim]")
{
    CHECK(trim(" \tabc") == "abc");
}

TEST_CASE("Trim string right", "[trim]")
{
    CHECK(trim("abc\t ") == "abc");
}

TEST_CASE("Trim left and right", "[trim]")
{
    CHECK(trim(" \tabc\t ") == "abc");
}

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

TEST_CASE("Parse string value with padding", "[value]")
{
    istringstream input(" abc\t\t\n");
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
    CHECK(value<double>(input) == Approx(45.6));
}

//----------------------------------------------------------------------------
// Cases for value() with range

TEST_CASE("Parse value with range", "[value]")
{
    istringstream input("55\n");
    CHECK(value<int>(input, 0, 100) == 55);
}

TEST_CASE("Parse value out of range", "[value]")
{
    istringstream input("55\n");
    CHECK_THROWS_AS(value<int>(input, 0, 10), runtime_error);
}

TEST_CASE("Parse value, range endpoints out of order", "[value]")
{
    istringstream input("55\n");
    CHECK(value<int>(input, 100, 0) == 55);
}

TEST_CASE("Parse value, range pair", "[value]")
{
    istringstream input("55\n55\n");
    CHECK(value<int>(input, 0, 100) ==
          value<int>(input, make_pair(0, 100)));
}

//----------------------------------------------------------------------------
// Cases for value() for enumerations

TEST_CASE("Parse enum value", "[value]")
{
    istringstream input("xyz\n");
    CHECK(value<int>(input, {
                { "xyz", 10 },
                { "abc", 12 }
            }) == 10);
}

TEST_CASE("Parse enum value, invalid token", "[value]")
{
    istringstream input("xyz\n");
    CHECK_THROWS_AS(value<int>(input, {
                { "abc", 20 },
                { "def", 30 }
            }), runtime_error);
}

//----------------------------------------------------------------------------
// Cases for match()

TEST_CASE("Empty match table", "[match]")
{
    istringstream input("xyz\n\nabc\n");
    static const unordered_map<string, function<void()> > empty;
    CHECK_THROWS_AS(match(input, empty), runtime_error);
    CHECK_THROWS_AS(match(input, empty), runtime_error);
    CHECK_THROWS_AS(match(input, empty), runtime_error);
}

TEST_CASE("Match table", "[match]")
{
    istringstream input("xyz\nabc\n");
    bool ok = false;
    match(input, { { "xyz", [&] { ok = true; } } });
    CHECK(ok);
}

//----------------------------------------------------------------------------
// Cases for value<fit::FileCreatorMesg>

TEST_CASE("Valid file_creator", "[file_creator][value]")
{
    istringstream input(
        "hardware_version\n"
        "150\n"
        "software_version\n"
        "330\n"
        "end\n"
        "file_creator\n"
        );
    CHECK_NOTHROW(value<fit::FileCreatorMesg>(input));
}

//----------------------------------------------------------------------------
// Cases for value<fit::FileIdMesg>

TEST_CASE("Valid file_id", "[file_id][value]")
{
    istringstream input(
        "time_created\n"
        "1457736704\n"
        "serial_number\n"
        "2501\n"
        "number\n"
        "5\n"
        "end\n"
        "file_id\n"
        );
    CHECK_NOTHROW(value<fit::FileIdMesg>(input));
}

//----------------------------------------------------------------------------
// Cases for value<fit::WorkoutMesg>

TEST_CASE("Valid workout", "[value][workout]")
{
    istringstream input(
        "wkt_name\n"
        "Tempo\n"
        "sport\n"
        "fishing\n"
        "num_valid_steps\n"
        "15\n"
        "capabilities\n"
        "31\n"
        "end\n"
        "workout\n"
        );
    CHECK_NOTHROW(value<fit::WorkoutMesg>(input));
}

//----------------------------------------------------------------------------
// Cases for value<fit::WorkoutStepMesg>

TEST_CASE("Valid workout_step", "[value][workout_step]")
{
    istringstream input(
        "wkt_step_name\n"
        "Intro\n"
        "intensity\n"
        "warmup\n"
        "duration_time\n"
        "31.5\n"
        "target_value\n"
        "0\n"
        "custom_target_heart_rate_low\n"
        "50\n"
        "custom_target_heart_rate_high\n"
        "270\n"
        "end\n"
        "workout_step\n"
        );
    CHECK_NOTHROW(value<fit::WorkoutStepMesg>(input));
}

//----------------------------------------------------------------------------
// Cases for xil2fit

TEST_CASE("Empty XIL input", "[xil2fit]")
{
    istringstream input;
    stringstream output;
    CHECK_NOTHROW(xil2fit(input, output));
    CHECK_FALSE(output.str().empty());
}

TEST_CASE("Invalid XIL input", "[xil2fit]")
{
    istringstream input(
        "begin\n"
        "nonsense\n"
        "end\n"
        "nonsense\n"
        );
    stringstream output;
    CHECK_THROWS_AS(xil2fit(input, output), runtime_error);
}

TEST_CASE("Valid XIL input", "[xil2fit]")
{
    istringstream input(
        "begin\n"
        "file_id\n"
        "end\n"
        "file_id\n"
        "begin\n"
        "file_creator\n"
        "end\n"
        "file_creator\n"
        "begin\n"
        "workout\n"
        "end\n"
        "workout\n"
        "begin\n"
        "workout_step\n"
        "end\n"
        "workout_step\n"
        );
    stringstream output;
    CHECK_NOTHROW(xil2fit(input, output));
    CHECK_FALSE(output.str().empty());
}

#endif  // _WITH_TESTS
