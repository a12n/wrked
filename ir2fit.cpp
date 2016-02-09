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

#define DEF_EXCEPTION(_name, _descr)            \
struct _name : public std::exception            \
{                                               \
    virtual const char*                         \
    what() const noexcept                       \
    {                                           \
        return _descr;                          \
    }                                           \
}

DEF_EXCEPTION(BadSyntax, "Syntax error");
DEF_EXCEPTION(EncodeFailed, "FIT encoder failed");
DEF_EXCEPTION(EndOfFile, "End of file");
DEF_EXCEPTION(InputFailed, "I/O error");

#undef DEF_EXCEPTION

//----------------------------------------------------------------------------


//----------------------------------------------------------------------------

#define DEF_STATE(_name)                                \
void _name (std::istream& input, fit::Encode& encode)

#define DECL_STATE(_name) DEF_STATE(_name)

#define NEXT_STATE(_name)                       \
_name(input, encode);                           \
return

DECL_STATE(start);
DECL_STATE(workout);
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

    // TODO

    NEXT_STATE(finish);
}

DEF_STATE(workout)
{
    // TODO
}

DEF_STATE(finish)
{
    try {
        // TODO
        throw EndOfFile();
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

void
ir2fit(std::istream& input, std::iostream& output)
{
    fit::Encode encode;

    encode.Open(output);

    start(input, encode);
}

} // namespace

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
