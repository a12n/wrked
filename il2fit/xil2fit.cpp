#include <algorithm>
#include <experimental/optional>
#include <functional>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>

#include <fit_date_time.hpp>
#include <fit_encode.hpp>
#include <fit_file_creator_mesg.hpp>
#include <fit_file_id_mesg.hpp>
#include <fit_workout_mesg.hpp>
#include <fit_workout_step_mesg.hpp>

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
    fit::WorkoutMesg ans;

    // Default values
    ans.SetSport(FIT_SPORT_CYCLING);
    ans.SetCapabilities(FIT_WORKOUT_CAPABILITIES_INVALID);
    ans.SetNumValidSteps(1);

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

    for (bool done = false; !done;) {
        match(input, {
                { "capabilities", [&] {
                        ans.SetCapabilities(value<FIT_WORKOUT_CAPABILITIES>(input)); } },
                { "num_valid_steps", [&] {
                        ans.SetNumValidSteps(value<FIT_UINT16>(input, 1, 10000)); } },
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
    fit::WorkoutStepMesg ans;

    // Default values
    ans.SetMessageIndex(FIT_MESSAGE_INDEX_INVALID);
    ans.SetDurationType(FIT_WKT_STEP_DURATION_OPEN);
    ans.SetTargetType(FIT_WKT_STEP_TARGET_OPEN);

    static const unordered_map<string, FIT_INTENSITY> intensities = {
        { "active"   , FIT_INTENSITY_ACTIVE   },
        { "rest"     , FIT_INTENSITY_REST     },
        { "warmup"   , FIT_INTENSITY_WARMUP   },
        { "cooldown" , FIT_INTENSITY_COOLDOWN }
    };

    for (bool done = false; !done;) {
        // TODO
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
    CHECK(value<double>(input) == 45.6);
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
    CHECK_THROWS_AS(match(input, {}), runtime_error);
    CHECK_THROWS_AS(match(input, {}), runtime_error);
    CHECK_THROWS_AS(match(input, {}), runtime_error);
}

TEST_CASE("Match table", "[match]")
{
    istringstream input("xyz\nabc\n");
    bool ok = false;
    match(input, { { "xyz", [&] { ok = true; } } });
    CHECK(ok);
}

#endif  // _WITH_TESTS
