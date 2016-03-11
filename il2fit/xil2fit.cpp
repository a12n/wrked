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
using std::experimental::nullopt;
using std::experimental::optional;
using std::function;
using std::get;
using std::getline;
using std::istream;
using std::istringstream;
using std::make_pair;
using std::pair;
using std::runtime_error;
using std::string;
using std::unordered_map;
using std::vector;

#define STR(_expr)                                                      \
    static_cast<std::ostringstream&>(                                   \
        std::ostringstream().flush() << _expr).str()

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

// struct line
// {
//     string str;
// 
//     operator string() const
//     {
//         return str;
//     }
// };
// 
// istream& operator>>(istream& input, line& ans)
// {
//     return getline(input, ans.str, '\n');
// }

optional<string>
line(istream& input)
{
    string ans;
    getline(input, ans, '\n');
    if (input.bad()) {
        error("I/O error");
    }
    return input.eof() ? none : some(ans);
}

//----------------------------------------------------------------------------
// xil2fit

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

template <>
std::string
value<std::string>(std::istream& input)
{
    return line(input).value();
}

typedef std::unordered_map<std::string, std::function<void()> > action_map;

#define ACTION(_token, _expr) action_map::value_type((_token), [&] { _expr; })

void
match(const std::string& token, const action_map& actions)
{
    try {
        actions.at(token)();
    } catch (const std::out_of_range&) {
        error("Bad action \"" + token + "\"");
    }
}

void
match(std::istream& input, const action_map& actions)
{
    match(value<std::string>(input), actions);
}

std::istream&
operator%=(std::istream& input, const action_map& actions)
{
    match(input, actions);
    return input;
}

template <>
fit::FileCreatorMesg
value<fit::FileCreatorMesg>(std::istream& input)
{
    fit::FileCreatorMesg ans;

    bool done = false;

    while (!done) {
        input %= {
            ACTION( "hardware_version", ans.SetHardwareVersion(value<FIT_UINT8>(input)) ),
            ACTION( "software_version", ans.SetSoftwareVersion(value<FIT_UINT16>(input)) ),
            { "end", [&] {
                    input %= {
                        { "file_creator", [&] { done = true; } }
                    }; } }
        };
    }

    return ans;
}

template <>
fit::FileIdMesg
value<fit::FileIdMesg>(std::istream& input)
{
    fit::FileIdMesg ans;

    // Default values
    ans.SetType(FIT_FILE_WORKOUT);
    ans.SetManufacturer(FIT_MANUFACTURER_GARMIN);
    ans.SetProduct(FIT_GARMIN_PRODUCT_EDGE500);
    ans.SetSerialNumber(54321);
    ans.SetTimeCreated(fit::DateTime(time(0)).GetTimeStamp());

    while (true) {
        const auto k = value<std::string>(input);
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
value<fit::WorkoutMesg>(std::istream& input)
{
    fit::WorkoutMesg ans;
    // TODO
    return ans;
}

template <>
fit::WorkoutStepMesg
value<fit::WorkoutStepMesg>(std::istream& input)
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
        const auto l = lopt.value();
        match(lopt.value(), {
                { "begin", [&] {
                        input %= {
                            ACTION( "file_creator",
                                    encode.Write(value<fit::FileCreatorMesg>(input))
                                ),
                            { "file_id", [&] {
                                    encode.Write(value<fit::FileIdMesg>(input));
                                } },
                            { "workout", [&] {
                                    encode.Write(value<fit::WorkoutMesg>(input));
                                } },
                            { "workout_step", [&] {
                                    encode.Write(value<fit::WorkoutStepMesg>(input));
                                } }
                        };
                    } }
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

TEST_CASE("EOF on empty line", "[line]")
{
    istringstream input;
    CHECK(line(input) == none);
}

#endif  // _WITH_TESTS
