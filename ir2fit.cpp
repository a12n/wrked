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

fit::FileIdMesg
fileIdMesg()
{
   fit::FileIdMesg ans;
   ans.SetType(FIT_FILE_WORKOUT);
   ans.SetManufacturer(FIT_MANUFACTURER_GARMIN);
   ans.SetProduct(FIT_GARMIN_PRODUCT_EDGE500);
   ans.SetSerialNumber(54321);
   ans.SetTimeCreated(0);
   return ans;
}

} // namespace

int
main()
{
    std::stringstream output(std::ios::out | std::ios::binary);
    std::string line;

    fit::Encode encode;

    encode.Open(output);
    encode.Write(fileIdMesg());

    try {
        while (std::getline(std::cin, line, '\n')) {
            // TODO
        }
    } catch (const std::exception& exn) {
        std::cerr << exn.what() << std::endl;
        return 1;
    }

    if (!encode.Close()) {
        std::cerr << "fit::Encode::Close()" << std::endl;
        return 1;
    }

    std::cout << output.str();

    return 0;
}
