#include <phonenumbers/phonenumberutil.h>
#include <phonenumbers/geocoding/phonenumber_offline_geocoder.h>

#include <erl_nif.h>
#include <string>
#include <vector>
#include <unicode/unistr.h>

#define UNUSED(expr) do { (void)(expr); } while (0)

using i18n::phonenumbers::PhoneNumberUtil;
using i18n::phonenumbers::PhoneNumber;
using i18n::phonenumbers::PhoneNumberOfflineGeocoder;

namespace {

struct phonenumber_data
{
    PhoneNumberOfflineGeocoder* geocoder;
};

// phone number tuple indexes

const int32_t kPhoneNumberHasCountryCodeIndex = 1;
const int32_t kPhoneNumberCountryCodeIndex = 2;
const int32_t kPhoneNumberHasNationalNumberIndex = 3;
const int32_t kPhoneNumberNationalNumberIndex = 4;
const int32_t kPhoneNumberHasExtensionIndex = 5;
const int32_t kPhoneNumberExtensionIndex = 6;
const int32_t kPhoneNumberHasNumberOfLeadingZerosIndex = 7;
const int32_t kPhoneNumberNumberOfLeadingZerosIndex = 8;
const int32_t kPhoneNumberHasItalianLeadingZeroIndex = 9;
const int32_t kPhoneNumberItalianLeadingZeroIndex = 10;
const int32_t kPhoneNumberHasRawInputIndex = 11;
const int32_t kPhoneNumberRawInputIndex = 12;
const int32_t kPhoneNumberHasCountryCodeSourceIndex = 13;
const int32_t kPhoneNumberCountryCodeSourceIndex = 14;
const int32_t kPhoneNumberHasPreferredDomesticCarrierCodeIndex = 15;
const int32_t kPhoneNumberPreferredDomesticCarrierCodeIndex = 16;

struct atoms
{
    ERL_NIF_TERM atomTrue;
    ERL_NIF_TERM atomFalse;

    ERL_NIF_TERM atomFixedLine;
    ERL_NIF_TERM atomMobile;
    ERL_NIF_TERM atomFixedLineOrMobile;
    ERL_NIF_TERM atomTollFree;
    ERL_NIF_TERM atomPremiumRate;
    ERL_NIF_TERM atomSharedCost;
    ERL_NIF_TERM atomVoip;
    ERL_NIF_TERM atomPersonalNumber;
    ERL_NIF_TERM atomPager;
    ERL_NIF_TERM atomUan;
    ERL_NIF_TERM atomVoiceMail;
    ERL_NIF_TERM atomUnknown;

    ERL_NIF_TERM atomIsPossible;
    ERL_NIF_TERM atomIsPossibleLocalOnly;
    ERL_NIF_TERM atomInvalidContryCode;
    ERL_NIF_TERM atomInvalidLength;
    ERL_NIF_TERM atomTooShort;
    ERL_NIF_TERM atomTooLong;

    ERL_NIF_TERM atomFromNumberUnspecified;
    ERL_NIF_TERM atomFromNumberWithPlusSign;
    ERL_NIF_TERM atomFromNumberWithIdd;
    ERL_NIF_TERM atomFromNumberWithoutPlusSign;
    ERL_NIF_TERM atomFromDefaultCountry;

    ERL_NIF_TERM atomInvalidNumber;
    ERL_NIF_TERM atomNoMatch;
    ERL_NIF_TERM atomShortNsmMatch;
    ERL_NIF_TERM atomNsmMatch;
    ERL_NIF_TERM atomExactMatch;

    ERL_NIF_TERM atomFormatE164;
    ERL_NIF_TERM atomFormatInternational;
    ERL_NIF_TERM atomFormatNational;
    ERL_NIF_TERM atomFormatRFC3966;

    ERL_NIF_TERM atomPhoneNumber;
} ATOMS;

inline ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;

    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1))
        return ret;

    return enif_make_atom(env, name);
}

inline bool get_binary(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifBinary* bin)
{
    if(enif_is_binary(env, term))
        return enif_inspect_binary(env, term, bin);

    return enif_inspect_iolist_as_binary(env, term, bin);
}

bool get_string(ErlNifEnv *env, ERL_NIF_TERM term, std::string* var)
{
    ErlNifBinary bin;

    if(get_binary(env, term, &bin))
    {
        *var = std::string(reinterpret_cast<const char*>(bin.data), bin.size);
        return true;
    }

    return false;
}

inline ERL_NIF_TERM make_binary(ErlNifEnv* env, const char* buff, size_t length)
{
    ERL_NIF_TERM term;
    uint8_t *destination_buffer = enif_make_new_binary(env, length, &term);
    memcpy(destination_buffer, buff, length);
    return term;
}

inline bool term_to_boolean(const ERL_NIF_TERM term, bool* boolean)
{
    if(enif_is_identical(term, ATOMS.atomTrue))
    {
        *boolean = true;
        return true;
    }

    if(enif_is_identical(term, ATOMS.atomFalse))
    {
        *boolean = false;
        return true;
    }

    return false;
}

inline ERL_NIF_TERM boolean_to_term(bool boolean)
{
    return boolean ? ATOMS.atomTrue : ATOMS.atomFalse;
}

bool term_to_phonenumber_format(const ERL_NIF_TERM term, PhoneNumberUtil::PhoneNumberFormat* number_format)
{
    bool return_value = true;

    if(enif_is_identical(ATOMS.atomFormatE164, term))
        *number_format = PhoneNumberUtil::E164;
    else if(enif_is_identical(ATOMS.atomFormatInternational, term))
        *number_format = PhoneNumberUtil::INTERNATIONAL;
    else if(enif_is_identical(ATOMS.atomFormatNational, term))
        *number_format = PhoneNumberUtil::NATIONAL;
    else if(enif_is_identical(ATOMS.atomFormatRFC3966, term))
        *number_format = PhoneNumberUtil::RFC3966;
    else
        return_value = false;

    return return_value;
}

bool term_to_phonenumber_country_code_source(const ERL_NIF_TERM term, PhoneNumber::CountryCodeSource* country_code_source)
{
    bool return_value = true;

    if(enif_is_identical(ATOMS.atomFromNumberWithPlusSign, term))
        *country_code_source = PhoneNumber::FROM_NUMBER_WITH_PLUS_SIGN;
    else if(enif_is_identical(ATOMS.atomFromNumberWithIdd, term))
        *country_code_source = PhoneNumber::FROM_NUMBER_WITH_IDD;
    else if(enif_is_identical(ATOMS.atomFromNumberWithoutPlusSign, term))
        *country_code_source = PhoneNumber::FROM_NUMBER_WITHOUT_PLUS_SIGN;
    else if(enif_is_identical(ATOMS.atomFromDefaultCountry, term))
        *country_code_source = PhoneNumber::FROM_DEFAULT_COUNTRY;
    else if(enif_is_identical(ATOMS.atomFromNumberUnspecified, term))
        *country_code_source = PhoneNumber::UNSPECIFIED;
    else
        return_value = false;

    return return_value;
}

bool term_to_phonenumber_type(const ERL_NIF_TERM term, PhoneNumberUtil::PhoneNumberType* type)
{
    bool return_value = true;

    if(enif_is_identical(ATOMS.atomFixedLine, term))
        *type = PhoneNumberUtil::FIXED_LINE;
    else if(enif_is_identical(ATOMS.atomMobile, term))
        *type = PhoneNumberUtil::MOBILE;
    else if(enif_is_identical(ATOMS.atomFixedLineOrMobile, term))
        *type = PhoneNumberUtil::FIXED_LINE_OR_MOBILE;
    else if(enif_is_identical(ATOMS.atomTollFree, term))
        *type = PhoneNumberUtil::TOLL_FREE;
    else if(enif_is_identical(ATOMS.atomPremiumRate, term))
        *type = PhoneNumberUtil::PREMIUM_RATE;
    else if(enif_is_identical(ATOMS.atomSharedCost, term))
        *type = PhoneNumberUtil::SHARED_COST;
    else if(enif_is_identical(ATOMS.atomVoip, term))
        *type = PhoneNumberUtil::VOIP;
    else if(enif_is_identical(ATOMS.atomPersonalNumber, term))
        *type = PhoneNumberUtil::PERSONAL_NUMBER;
    else if(enif_is_identical(ATOMS.atomPager, term))
        *type = PhoneNumberUtil::PAGER;
    else if(enif_is_identical(ATOMS.atomUan, term))
            *type = PhoneNumberUtil::UAN;
    else if(enif_is_identical(ATOMS.atomVoiceMail, term))
        *type = PhoneNumberUtil::VOICEMAIL;
    else if(enif_is_identical(ATOMS.atomUnknown, term))
        *type = PhoneNumberUtil::UNKNOWN;
    else
        return_value = false;

    return return_value;
}

ERL_NIF_TERM phonenumber_type_to_term(PhoneNumberUtil::PhoneNumberType type)
{
    switch(type)
    {
        case PhoneNumberUtil::FIXED_LINE:
            return ATOMS.atomFixedLine;
        case PhoneNumberUtil::MOBILE:
            return ATOMS.atomMobile;
        case PhoneNumberUtil::FIXED_LINE_OR_MOBILE:
            return ATOMS.atomFixedLineOrMobile;
        case PhoneNumberUtil::TOLL_FREE:
            return ATOMS.atomTollFree;
        case PhoneNumberUtil::PREMIUM_RATE:
            return ATOMS.atomPremiumRate;
        case PhoneNumberUtil::SHARED_COST:
            return ATOMS.atomSharedCost;
        case PhoneNumberUtil::VOIP:
            return ATOMS.atomVoip;
        case PhoneNumberUtil::PERSONAL_NUMBER:
            return ATOMS.atomPersonalNumber;
        case PhoneNumberUtil::PAGER:
            return ATOMS.atomPager;
        case PhoneNumberUtil::UAN:
            return ATOMS.atomUan;
        case PhoneNumberUtil::VOICEMAIL:
            return ATOMS.atomVoiceMail;
        case PhoneNumberUtil::UNKNOWN:
            return ATOMS.atomUnknown;
    }

    return ATOMS.atomUnknown;
}

ERL_NIF_TERM phonenumber_validation_result_to_term(PhoneNumberUtil::ValidationResult validation_result)
{
    switch(validation_result)
    {
        case PhoneNumberUtil::IS_POSSIBLE:
            return ATOMS.atomIsPossible;
        case PhoneNumberUtil::IS_POSSIBLE_LOCAL_ONLY:
            return ATOMS.atomIsPossibleLocalOnly;
        case PhoneNumberUtil::INVALID_COUNTRY_CODE:
            return ATOMS.atomInvalidContryCode;
        case PhoneNumberUtil::TOO_SHORT:
            return ATOMS.atomTooShort;
        case PhoneNumberUtil::INVALID_LENGTH:
            return ATOMS.atomInvalidLength;
        case PhoneNumberUtil::TOO_LONG:
            return ATOMS.atomTooLong;
    }

    return ATOMS.atomInvalidContryCode;
}

ERL_NIF_TERM phonenumber_country_code_source_to_term(PhoneNumber::CountryCodeSource country_code_source)
{
    switch(country_code_source)
    {
        case PhoneNumber::FROM_NUMBER_WITH_PLUS_SIGN:
            return ATOMS.atomFromNumberWithPlusSign;
        case PhoneNumber::FROM_NUMBER_WITH_IDD:
            return ATOMS.atomFromNumberWithIdd;
        case PhoneNumber::FROM_NUMBER_WITHOUT_PLUS_SIGN:
            return ATOMS.atomFromNumberWithoutPlusSign;
        case PhoneNumber::FROM_DEFAULT_COUNTRY:
            return ATOMS.atomFromDefaultCountry;
        case PhoneNumber::UNSPECIFIED:
            return ATOMS.atomFromNumberUnspecified;
    }

    return ATOMS.atomFromNumberUnspecified;
}

ERL_NIF_TERM phonenumber_match_type_to_term(PhoneNumberUtil::MatchType match_type)
{
    switch(match_type)
    {
        case PhoneNumberUtil::INVALID_NUMBER:
            return ATOMS.atomInvalidNumber;
        case PhoneNumberUtil::NO_MATCH:
            return ATOMS.atomNoMatch;
        case PhoneNumberUtil::SHORT_NSN_MATCH:
            return ATOMS.atomShortNsmMatch;
        case PhoneNumberUtil::NSN_MATCH:
            return ATOMS.atomNsmMatch;
        case PhoneNumberUtil::EXACT_MATCH:
            return ATOMS.atomExactMatch;
    }

    return ATOMS.atomInvalidNumber;
}

ERL_NIF_TERM phonenumber_to_term(ErlNifEnv* env, PhoneNumber phoneNumber)
{
    // country_code

    ERL_NIF_TERM has_country_code = boolean_to_term(phoneNumber.has_country_code());
    ERL_NIF_TERM country_code = enif_make_int(env, phoneNumber.country_code());

    // national_number

    ERL_NIF_TERM has_national_number = boolean_to_term(phoneNumber.has_national_number());
    ERL_NIF_TERM national_number = enif_make_uint64(env, phoneNumber.national_number());

    // extension

    ERL_NIF_TERM has_extension = boolean_to_term(phoneNumber.has_extension());
    ERL_NIF_TERM extension = make_binary(env, phoneNumber.extension().c_str(), phoneNumber.extension().size());

    // number_of_leading_zeros

    ERL_NIF_TERM has_number_of_leading_zeros = boolean_to_term(phoneNumber.has_number_of_leading_zeros());
    ERL_NIF_TERM number_of_leading_zeros = enif_make_int(env, phoneNumber.number_of_leading_zeros());

    // italian_leading_zero;

    ERL_NIF_TERM has_italian_leading_zero = boolean_to_term(phoneNumber.has_italian_leading_zero());
    ERL_NIF_TERM italian_leading_zero = boolean_to_term(phoneNumber.italian_leading_zero());

    // raw_input

    ERL_NIF_TERM has_raw_input = boolean_to_term(phoneNumber.has_raw_input());
    ERL_NIF_TERM raw_input = make_binary(env, phoneNumber.raw_input().c_str(), phoneNumber.raw_input().size());

    // country_code_source;

    ERL_NIF_TERM has_country_code_source = boolean_to_term(phoneNumber.has_country_code_source());
    ERL_NIF_TERM country_code_source = phonenumber_country_code_source_to_term(phoneNumber.country_code_source());

    // preferred_domestic_carrier_code
    ERL_NIF_TERM has_preferred_domestic_carrier_code = boolean_to_term(phoneNumber.has_preferred_domestic_carrier_code());
    ERL_NIF_TERM preferred_domestic_carrier_code = make_binary(env, phoneNumber.preferred_domestic_carrier_code().c_str(), phoneNumber.preferred_domestic_carrier_code().size());

    return enif_make_tuple(env, 17,
        ATOMS.atomPhoneNumber,
        has_country_code,
        country_code,
        has_national_number,
        national_number,
        has_extension,
        extension,
        has_number_of_leading_zeros,
        number_of_leading_zeros,
        has_italian_leading_zero,
        italian_leading_zero,
        has_raw_input,
        raw_input,
        has_country_code_source,
        country_code_source,
        has_preferred_domestic_carrier_code,
        preferred_domestic_carrier_code);
}

bool term_to_phonenumber(ErlNifEnv* env, const ERL_NIF_TERM term, PhoneNumber* phoneNumber)
{
    int32_t array_count;
    const ERL_NIF_TERM* array;

    if (!enif_get_tuple(env, term, &array_count, &array) || array_count != 17)
        return false;

    // country_code

    if(enif_is_identical(array[kPhoneNumberHasCountryCodeIndex], ATOMS.atomTrue))
    {
        int32_t country_code;

        if (!enif_get_int(env, array[kPhoneNumberCountryCodeIndex], &country_code))
            return false;

        phoneNumber->set_country_code(country_code);
    }

    // national_number

    if(enif_is_identical(array[kPhoneNumberHasNationalNumberIndex], ATOMS.atomTrue))
    {
        unsigned long national_number;

        if (!enif_get_uint64(env, array[kPhoneNumberNationalNumberIndex], &national_number))
            return false;

        phoneNumber->set_national_number(national_number);
    }

    // extension

    if(enif_is_identical(array[kPhoneNumberHasExtensionIndex], ATOMS.atomTrue))
    {
        ErlNifBinary bin;

        if(!get_binary(env, array[kPhoneNumberExtensionIndex], &bin))
            return false;

        phoneNumber->set_extension(reinterpret_cast<const char*>(bin.data), bin.size);
    }

    // number_of_leading_zeros

    if(enif_is_identical(array[kPhoneNumberHasNumberOfLeadingZerosIndex], ATOMS.atomTrue))
    {
        int32_t number_of_leading_zeros;

        if (!enif_get_int(env, array[kPhoneNumberNumberOfLeadingZerosIndex], &number_of_leading_zeros))
            return false;

        phoneNumber->set_number_of_leading_zeros(number_of_leading_zeros);
    }

    // italian_leading_zero

    if(enif_is_identical(array[kPhoneNumberHasItalianLeadingZeroIndex], ATOMS.atomTrue))
    {
        bool italian_leading_zero;

        if (!term_to_boolean(array[kPhoneNumberItalianLeadingZeroIndex], &italian_leading_zero))
            return false;

        phoneNumber->set_italian_leading_zero(italian_leading_zero);
    }

    // raw_input

    if(enif_is_identical(array[kPhoneNumberHasRawInputIndex], ATOMS.atomTrue))
    {
        ErlNifBinary bin;

        if(!get_binary(env, array[kPhoneNumberRawInputIndex], &bin))
            return false;

        phoneNumber->set_raw_input(reinterpret_cast<const char*>(bin.data), bin.size);
    }

    // country_code_source

    if(enif_is_identical(array[kPhoneNumberHasCountryCodeSourceIndex], ATOMS.atomTrue))
    {
        PhoneNumber::CountryCodeSource country_code_source;

        if (!term_to_phonenumber_country_code_source(array[kPhoneNumberCountryCodeSourceIndex], &country_code_source))
            return false;

        phoneNumber->set_country_code_source(country_code_source);
    }

    // preferred_domestic_carrier_code

    if(enif_is_identical(array[kPhoneNumberHasPreferredDomesticCarrierCodeIndex], ATOMS.atomTrue))
    {
        ErlNifBinary bin;

        if(!get_binary(env, array[kPhoneNumberPreferredDomesticCarrierCodeIndex], &bin))
            return false;

        phoneNumber->set_preferred_domestic_carrier_code(reinterpret_cast<const char*>(bin.data), bin.size);
    }

    return true;
}

}  // namespace

int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(load_info);

    ATOMS.atomTrue = make_atom(env, "true");
    ATOMS.atomFalse = make_atom(env, "false");

    ATOMS.atomFixedLine = make_atom(env, "fixed_line");
    ATOMS.atomMobile = make_atom(env, "mobile");
    ATOMS.atomFixedLineOrMobile = make_atom(env, "fixed_line_or_mobile");
    ATOMS.atomTollFree = make_atom(env, "toll_free");
    ATOMS.atomPremiumRate = make_atom(env, "premium_rate");
    ATOMS.atomSharedCost = make_atom(env, "shared_cost");
    ATOMS.atomVoip = make_atom(env, "voip");
    ATOMS.atomPersonalNumber = make_atom(env, "personal_number");
    ATOMS.atomPager = make_atom(env, "pager");
    ATOMS.atomUan = make_atom(env, "uan");
    ATOMS.atomVoiceMail = make_atom(env, "voicemail");
    ATOMS.atomUnknown = make_atom(env, "unknown");

    ATOMS.atomIsPossible = make_atom(env, "is_possible");
    ATOMS.atomIsPossibleLocalOnly = make_atom(env, "is_possible_local_only");
    ATOMS.atomInvalidContryCode = make_atom(env, "invalid_country_code");
    ATOMS.atomInvalidLength = make_atom(env, "invalid_length");
    ATOMS.atomTooShort = make_atom(env, "too_short");
    ATOMS.atomTooLong = make_atom(env, "too_long");

    ATOMS.atomFromNumberWithPlusSign = make_atom(env, "from_number_with_plus_sign");
    ATOMS.atomFromNumberWithIdd = make_atom(env, "from_number_with_idd");
    ATOMS.atomFromNumberWithoutPlusSign = make_atom(env, "from_number_without_plus_sign");
    ATOMS.atomFromDefaultCountry = make_atom(env, "from_default_country");
    ATOMS.atomFromNumberUnspecified = make_atom(env, "unspecified");

    ATOMS.atomInvalidNumber = make_atom(env, "invalid_number");
    ATOMS.atomNoMatch = make_atom(env, "no_match");
    ATOMS.atomShortNsmMatch = make_atom(env, "short_nsn_match");
    ATOMS.atomNsmMatch = make_atom(env, "nsn_match");
    ATOMS.atomExactMatch = make_atom(env, "exact_match");

    ATOMS.atomFormatE164 = make_atom(env, "e164");
    ATOMS.atomFormatInternational = make_atom(env, "international");
    ATOMS.atomFormatNational = make_atom(env, "national");
    ATOMS.atomFormatRFC3966 = make_atom(env, "rfc3966");

    ATOMS.atomPhoneNumber = make_atom(env, "phonenumber");

    phonenumber_data* data = static_cast<phonenumber_data*>(enif_alloc(sizeof(phonenumber_data)));
    data->geocoder = new PhoneNumberOfflineGeocoder();
    *priv_data = data;

    return 0;
}

void on_nif_unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);

    phonenumber_data* data = static_cast<phonenumber_data*>(priv_data);

    if(data->geocoder)
        delete data->geocoder;

    enif_free(data);
}

int on_nif_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    UNUSED(old_priv);
    UNUSED(info);

    phonenumber_data* old_data = static_cast<phonenumber_data*>(*old_priv);
    phonenumber_data* data = static_cast<phonenumber_data*>(enif_alloc(sizeof(phonenumber_data)));

    data->geocoder = old_data->geocoder;
    old_data->geocoder = nullptr;

    *priv = data;
    return 0;
}

// NIF functions -> Geocoding

static ERL_NIF_TERM GetGeocodingForNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    std::string locale_str;

    if(!get_string(env, argv[1], &locale_str))
        return enif_make_badarg(env);

    icu::Locale locale(locale_str.c_str());

    phonenumber_data* data = static_cast<phonenumber_data*>(enif_priv_data(env));
    std::string description = data->geocoder->GetDescriptionForValidNumber(phoneNumber, locale);
    return make_binary(env, description.c_str(), description.size());
}

// NIF functions -> PhoneNumberUtil

static ERL_NIF_TERM GetSupportedRegions_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    std::set<std::string> regions;
    PhoneNumberUtil::GetInstance()->GetSupportedRegions(&regions);

    std::vector<ERL_NIF_TERM> array;
    array.reserve(regions.size());

    for (auto const& value : regions)
        array.push_back(make_binary(env, value.c_str(), value.size()));

    return enif_make_list_from_array(env, array.data(), array.size());
}

static ERL_NIF_TERM IsAlphaNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string number;

    if(!get_string(env, argv[0], &number))
        return enif_make_badarg(env);

    return boolean_to_term(PhoneNumberUtil::GetInstance()->IsAlphaNumber(number));
}

static ERL_NIF_TERM ConvertAlphaCharactersInNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string number;

    if(!get_string(env, argv[0], &number))
        return enif_make_badarg(env);

    PhoneNumberUtil::GetInstance()->ConvertAlphaCharactersInNumber(&number);
    return make_binary(env, number.c_str(), number.size());
}

static ERL_NIF_TERM NormalizeDigitsOnly_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string number;

    if(!get_string(env, argv[0], &number))
        return enif_make_badarg(env);

    PhoneNumberUtil::GetInstance()->NormalizeDigitsOnly(&number);
    return make_binary(env, number.c_str(), number.size());
}

static ERL_NIF_TERM NormalizeDiallableCharsOnly_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string number;

    if(!get_string(env, argv[0], &number))
        return enif_make_badarg(env);

    PhoneNumberUtil::GetInstance()->NormalizeDiallableCharsOnly(&number);
    return make_binary(env, number.c_str(), number.size());
}

static ERL_NIF_TERM GetNationalSignificantNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    std::string national_number;
    PhoneNumberUtil::GetInstance()->GetNationalSignificantNumber(phoneNumber, &national_number);
    return make_binary(env, national_number.c_str(), national_number.size());
}

static ERL_NIF_TERM GetLengthOfGeographicalAreaCode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    return enif_make_int(env, PhoneNumberUtil::GetInstance()->GetLengthOfGeographicalAreaCode(phoneNumber));
}

static ERL_NIF_TERM GetLengthOfNationalDestinationCode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    return enif_make_int(env, PhoneNumberUtil::GetInstance()->GetLengthOfNationalDestinationCode(phoneNumber));
}

static ERL_NIF_TERM GetCountryMobileToken_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    int code;

    if (!enif_get_int(env, argv[0], &code))
        return enif_make_badarg(env);

    std::string mobile_token;

    PhoneNumberUtil::GetInstance()->GetCountryMobileToken(code, &mobile_token);
    return make_binary(env, mobile_token.c_str(), mobile_token.size());
}

static ERL_NIF_TERM Format_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    PhoneNumberUtil::PhoneNumberFormat phoneNumberFormat;

    if (!term_to_phonenumber_format(argv[1], &phoneNumberFormat))
        return enif_make_badarg(env);

    std::string formatted_number;
    PhoneNumberUtil::GetInstance()->Format(phoneNumber, phoneNumberFormat, &formatted_number);
    return make_binary(env, formatted_number.c_str(), formatted_number.size());
}

// FormatByPattern

static ERL_NIF_TERM FormatNationalNumberWithCarrierCode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    std::string carrier_code;

    if(!get_string(env, argv[1], &carrier_code))
        return enif_make_badarg(env);

    std::string formatted_number;
    PhoneNumberUtil::GetInstance()->FormatNationalNumberWithCarrierCode(phoneNumber, carrier_code, &formatted_number);
    return make_binary(env, formatted_number.c_str(), formatted_number.size());
}

static ERL_NIF_TERM FormatNationalNumberWithPreferredCarrierCode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    std::string fb_carrier_code;

    if(!get_string(env, argv[1], &fb_carrier_code))
        return enif_make_badarg(env);

    std::string formatted_number;
    PhoneNumberUtil::GetInstance()->FormatNationalNumberWithPreferredCarrierCode(phoneNumber, fb_carrier_code, &formatted_number);
    return make_binary(env, formatted_number.c_str(), formatted_number.size());
}

static ERL_NIF_TERM FormatNumberForMobileDialing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    std::string region_calling_from;

    if(!get_string(env, argv[1], &region_calling_from))
        return enif_make_badarg(env);

    bool with_formatting;

    if (!term_to_boolean(argv[2], &with_formatting))
        return enif_make_badarg(env);

    std::string formatted_number;
    PhoneNumberUtil::GetInstance()->FormatNumberForMobileDialing(phoneNumber, region_calling_from, with_formatting, &formatted_number);
    return make_binary(env, formatted_number.c_str(), formatted_number.size());
}

static ERL_NIF_TERM FormatOutOfCountryCallingNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    std::string calling_from;

    if(!get_string(env, argv[1], &calling_from))
        return enif_make_badarg(env);

    std::string formatted_number;
    PhoneNumberUtil::GetInstance()->FormatOutOfCountryCallingNumber(phoneNumber, calling_from, &formatted_number);
    return make_binary(env, formatted_number.c_str(), formatted_number.size());
}

static ERL_NIF_TERM FormatInOriginalFormat_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    std::string region_code;

    if(!get_string(env, argv[1], &region_code))
        return enif_make_badarg(env);

    std::string formatted_number;
    PhoneNumberUtil::GetInstance()->FormatInOriginalFormat(phoneNumber, region_code, &formatted_number);
    return make_binary(env, formatted_number.c_str(), formatted_number.size());
}

static ERL_NIF_TERM FormatOutOfCountryKeepingAlphaChars_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    std::string calling_from;

    if(!get_string(env, argv[1], &calling_from))
        return enif_make_badarg(env);

    std::string formatted_number;
    PhoneNumberUtil::GetInstance()->FormatOutOfCountryKeepingAlphaChars(phoneNumber, calling_from, &formatted_number);
    return make_binary(env, formatted_number.c_str(), formatted_number.size());
}

static ERL_NIF_TERM TruncateTooLongNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    PhoneNumberUtil::GetInstance()->TruncateTooLongNumber(&phoneNumber);
    return phonenumber_to_term(env, phoneNumber);
}

static ERL_NIF_TERM GetNumberType_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    return phonenumber_type_to_term(PhoneNumberUtil::GetInstance()->GetNumberType(phoneNumber));
}

static ERL_NIF_TERM IsValidNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    return boolean_to_term(PhoneNumberUtil::GetInstance()->IsValidNumber(phoneNumber));
}

static ERL_NIF_TERM IsValidNumberForRegion_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    std::string region_code;

    if(!get_string(env, argv[1], &region_code))
        return enif_make_badarg(env);

    return boolean_to_term(PhoneNumberUtil::GetInstance()->IsValidNumberForRegion(phoneNumber, region_code));
}

static ERL_NIF_TERM GetRegionCodeForNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    std::string region_code;
    PhoneNumberUtil::GetInstance()->GetRegionCodeForNumber(phoneNumber, &region_code);

    return make_binary(env, region_code.c_str(), region_code.size());
}

static ERL_NIF_TERM GetCountryCodeForRegion_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string region_code;

    if(!get_string(env, argv[0], &region_code))
        return enif_make_badarg(env);

    return enif_make_int(env, PhoneNumberUtil::GetInstance()->GetCountryCodeForRegion(region_code));
}

static ERL_NIF_TERM GetRegionCodeForCountryCode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    int code;

    if (!enif_get_int(env, argv[0], &code))
        return enif_make_badarg(env);

    std::string region_code;
    PhoneNumberUtil::GetInstance()->GetRegionCodeForCountryCode(code, &region_code);
    return make_binary(env, region_code.c_str(), region_code.size());
}

static ERL_NIF_TERM GetRegionCodesForCountryCallingCode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    int code;

    if (!enif_get_int(env, argv[0], &code))
        return enif_make_badarg(env);

    std::list<std::string> regions;
    PhoneNumberUtil::GetInstance()->GetRegionCodesForCountryCallingCode(code, &regions);

    std::vector<ERL_NIF_TERM> array;
    array.reserve(regions.size());

    for (auto const& value : regions)
        array.push_back(make_binary(env, value.c_str(), value.size()));

    return enif_make_list_from_array(env, array.data(), array.size());
}

static ERL_NIF_TERM GetRegionDisplayName_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    std::string region_code;
    std::string locale_str;

    if(!get_string(env, argv[0], &region_code) || !get_string(env, argv[1], &locale_str))
        return enif_make_badarg(env);

    std::string display_country;

    if (region_code.compare("ZZ") != 0 && region_code.compare(PhoneNumberUtil::kRegionCodeForNonGeoEntity) != 0)
    {
        icu::Locale language(locale_str.c_str());
        icu::UnicodeString udisplay_country;
        icu::Locale("", region_code.c_str()).getDisplayCountry(language, udisplay_country);
        udisplay_country.toUTF8String(display_country);
    }

    return make_binary(env, display_country.c_str(), display_country.size());
}

static ERL_NIF_TERM IsNANPACountry_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string region_code;

    if(!get_string(env, argv[0], &region_code))
        return enif_make_badarg(env);

    return boolean_to_term(PhoneNumberUtil::GetInstance()->IsNANPACountry(region_code));
}

static ERL_NIF_TERM GetNddPrefixForRegion_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string region_code;

    if(!get_string(env, argv[0], &region_code))
        return enif_make_badarg(env);

    bool stripNonDigits;

    if (!term_to_boolean(argv[1], &stripNonDigits))
        return enif_make_badarg(env);

    std::string national_prefix;
    PhoneNumberUtil::GetInstance()->GetNddPrefixForRegion(region_code, stripNonDigits, &national_prefix);
    return make_binary(env, national_prefix.c_str(), national_prefix.size());
}

static ERL_NIF_TERM IsPossibleNumberWithReason_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    return phonenumber_validation_result_to_term(PhoneNumberUtil::GetInstance()->IsPossibleNumberWithReason(phoneNumber));
}

static ERL_NIF_TERM IsPossibleNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    return boolean_to_term( PhoneNumberUtil::GetInstance()->IsPossibleNumber(phoneNumber));
}

static ERL_NIF_TERM IsPossibleNumberForString_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string number;

    if(!get_string(env, argv[0], &number))
        return enif_make_badarg(env);

    std::string region_dialing_from;

    if(!get_string(env, argv[1], &region_dialing_from))
        return enif_make_badarg(env);

    return boolean_to_term(PhoneNumberUtil::GetInstance()->IsPossibleNumberForString(number, region_dialing_from));
}

static ERL_NIF_TERM CanBeInternationallyDialled_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    return boolean_to_term(PhoneNumberUtil::GetInstance()->CanBeInternationallyDialled(phoneNumber));
}

static ERL_NIF_TERM IsNumberGeographical_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber))
        return enif_make_badarg(env);

    return boolean_to_term(PhoneNumberUtil::GetInstance()->IsNumberGeographical(phoneNumber));
}

static ERL_NIF_TERM GetExampleNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string region_code;

    if(!get_string(env, argv[0], &region_code))
        return enif_make_badarg(env);

    PhoneNumber phoneNumber;

    if (!PhoneNumberUtil::GetInstance()->GetExampleNumber(region_code, &phoneNumber))
        return ATOMS.atomFalse;

    return phonenumber_to_term(env, phoneNumber);
}

static ERL_NIF_TERM GetExampleNumberForType_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string region_code;

    if(!get_string(env, argv[0], &region_code))
        return enif_make_badarg(env);

    PhoneNumberUtil::PhoneNumberType type;

    if (!term_to_phonenumber_type(argv[1], &type))
        return enif_make_badarg(env);

    PhoneNumber phoneNumber;

    if (!PhoneNumberUtil::GetInstance()->GetExampleNumberForType(region_code, type, &phoneNumber))
        return ATOMS.atomFalse;

    return phonenumber_to_term(env, phoneNumber);
}

static ERL_NIF_TERM GetExampleNumberForNonGeoEntity_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    int code;

    if (!enif_get_int(env, argv[0], &code))
        return enif_make_badarg(env);

    PhoneNumber phoneNumber;

    if (!PhoneNumberUtil::GetInstance()->GetExampleNumberForNonGeoEntity(code, &phoneNumber))
        return ATOMS.atomFalse;

    return phonenumber_to_term(env, phoneNumber);
}

static ERL_NIF_TERM Parse_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string number_to_parse;

    if(!get_string(env, argv[0], &number_to_parse))
        return enif_make_badarg(env);

    std::string region_code;

    if(!get_string(env, argv[1], &region_code))
        return enif_make_badarg(env);

    PhoneNumber phoneNumber;
    PhoneNumberUtil::GetInstance()->Parse(number_to_parse, region_code, &phoneNumber);
    return phonenumber_to_term(env, phoneNumber);
}

static ERL_NIF_TERM ParseAndKeepRawInput_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string number_to_parse;

    if(!get_string(env, argv[0], &number_to_parse))
        return enif_make_badarg(env);

    std::string region_code;

    if(!get_string(env, argv[1], &region_code))
        return enif_make_badarg(env);

    PhoneNumber phoneNumber;
    PhoneNumberUtil::GetInstance()->ParseAndKeepRawInput(number_to_parse, region_code, &phoneNumber);
    return phonenumber_to_term(env, phoneNumber);
}

static ERL_NIF_TERM IsNumberMatch_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber1;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber1))
        return enif_make_badarg(env);

    PhoneNumber phoneNumber2;

    if (!term_to_phonenumber(env, argv[1], &phoneNumber2))
        return enif_make_badarg(env);

    return phonenumber_match_type_to_term(PhoneNumberUtil::GetInstance()->IsNumberMatch(phoneNumber1, phoneNumber2));
}

static ERL_NIF_TERM IsNumberMatchWithTwoStrings_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    std::string first_number;

    if(!get_string(env, argv[0], &first_number))
        return enif_make_badarg(env);

    std::string second_number;

    if(!get_string(env, argv[1], &second_number))
        return enif_make_badarg(env);

    return phonenumber_match_type_to_term(PhoneNumberUtil::GetInstance()->IsNumberMatchWithTwoStrings(first_number, second_number));
}

static ERL_NIF_TERM IsNumberMatchWithOneString_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    PhoneNumber phoneNumber1;

    if (!term_to_phonenumber(env, argv[0], &phoneNumber1))
        return enif_make_badarg(env);

    std::string second_number;

    if(!get_string(env, argv[1], &second_number))
        return enif_make_badarg(env);

    return phonenumber_match_type_to_term(PhoneNumberUtil::GetInstance()->IsNumberMatchWithOneString(phoneNumber1, second_number));
}

static ErlNifFunc nif_funcs[] =
{
    // PhoneNumberOfflineGeocoder

    {"get_geocoding_for_number", 2, GetGeocodingForNumber_nif},

    // PhoneNumberUtil

    {"get_supported_regions", 0, GetSupportedRegions_nif},
    {"is_alpha_number", 1, IsAlphaNumber_nif},
    {"convert_alpha_characters_in_number", 1, ConvertAlphaCharactersInNumber_nif},
    {"normalize_digits_only", 1, NormalizeDigitsOnly_nif},
    {"normalize_diallable_chars_only", 1, NormalizeDiallableCharsOnly_nif},
    {"get_national_significant_number", 1, GetNationalSignificantNumber_nif},
    {"get_length_of_geograpical_area_code", 1, GetLengthOfGeographicalAreaCode_nif},
    {"get_length_of_national_destination_code", 1, GetLengthOfNationalDestinationCode_nif},
    {"get_country_mobile_token", 1, GetCountryMobileToken_nif},
    {"format", 2, Format_nif},
    {"format_national_number_with_carrier_code", 2, FormatNationalNumberWithCarrierCode_nif},
    {"format_national_number_with_preferred_carrier_code", 2, FormatNationalNumberWithPreferredCarrierCode_nif},
    {"format_number_for_mobile_dialing", 3, FormatNumberForMobileDialing_nif},
    {"format_out_of_country_calling_number", 2, FormatOutOfCountryCallingNumber_nif},
    {"format_in_original_format", 2, FormatInOriginalFormat_nif},
    {"format_out_of_country_keeping_alpha_chars", 2, FormatOutOfCountryKeepingAlphaChars_nif},
    {"truncate_too_long_number", 1, TruncateTooLongNumber_nif},
    {"get_number_type", 1, GetNumberType_nif},
    {"is_valid_number", 1, IsValidNumber_nif},
    {"is_valid_number_for_region", 2, IsValidNumberForRegion_nif},
    {"get_region_code_for_number", 1, GetRegionCodeForNumber_nif},
    {"get_country_code_for_region", 1, GetCountryCodeForRegion_nif},
    {"get_region_code_for_country_code", 1, GetRegionCodeForCountryCode_nif},
    {"get_region_codes_for_country_calling_code", 1, GetRegionCodesForCountryCallingCode_nif},
    {"get_region_display_name", 2, GetRegionDisplayName_nif},
    {"is_nanpa_country", 1, IsNANPACountry_nif},
    {"get_ndd_prefix_for_region", 2, GetNddPrefixForRegion_nif},
    {"is_possible_number_with_reason", 1, IsPossibleNumberWithReason_nif},
    {"is_possible_number", 1, IsPossibleNumber_nif},
    {"is_possible_number_for_string", 2, IsPossibleNumberForString_nif},
    {"can_be_internationally_dialled", 1, CanBeInternationallyDialled_nif},
    {"is_number_geographical", 1, IsNumberGeographical_nif},
    {"get_example_number", 1, GetExampleNumber_nif},
    {"get_example_number_for_type", 2, GetExampleNumberForType_nif},
    {"get_example_number_for_non_geo_entity", 1, GetExampleNumberForNonGeoEntity_nif},
    {"parse", 2, Parse_nif},
    {"parse_and_keep_raw_input", 2, ParseAndKeepRawInput_nif},
    {"is_number_match", 2, IsNumberMatch_nif},
    {"is_number_match_with_two_strings", 2, IsNumberMatchWithTwoStrings_nif},
    {"is_number_match_with_one_string", 2, IsNumberMatchWithOneString_nif}
};

ERL_NIF_INIT(phonenumber_util, nif_funcs, on_nif_load, NULL, on_nif_upgrade, on_nif_unload)
