#include <ctime>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include <fit_date_time.hpp>
#include <fit_encode.hpp>
#include <fit_file_creator_mesg.hpp>
#include <fit_file_id_mesg.hpp>
#include <fit_workout_mesg.hpp>
#include <fit_workout_step_mesg.hpp>

namespace {

//----------------------------------------------------------------------------
// xil2fit

void
xil2fit(std::istream& input, std::iostream& output)
{
    fit::Encode encode;

    encode.Open(output);

    // TODO

    if (!encode.Close()) {
        throw std::runtime_error("FIT encoder failed");
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

// TODO

#endif  // _WITH_TESTS
