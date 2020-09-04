#ifndef EXCONFIG_H_
#define EXCONFIG_H_

#include <stdint.h>
#include <time.h>
#include <arpa/inet.h>

#include <ei.h>
#ifndef HAS_EI_INIT
#include <erl_interface.h>
#endif

#include <vector>
#include <string>
#include <set>
#include <xconfig/xconfig.h>

using xconfig::XConfigNode;

/* Timeout for the initial connection to xconfigd */
const int CONNECTION_TIMEOUT = 5;

/* Erlang will send the packet size as a 32-bit big endian integer.
   When opening the port in the Erlang side, the options
   [binary, {packet, 4}]
   MUST be specified
*/
typedef uint32_t packet_size_t;

/* Functions to convert from/to big-endian <-> native-endian */
inline uint32_t packet_length_swap_in(uint32_t n) {
    // Big-endian to native-endian
    return ntohl(n);
}

inline uint32_t packet_length_swap_out(uint32_t n) {
    // Native endian to big-endian
    return htonl(n);
}

// Maximum packet size that we accept as input from Erlang
const size_t MAX_PACKET_SIZE = 65536;


/* Non-recoverable exception, thrown when we lost synchronization of the
   stream with Erlang.
   The driver should exit */
class StreamException : public std::runtime_error {
public:
    StreamException(const std::string& msg) : std::runtime_error(msg) {}
};


/* Recoverable exception. We can return the error to Erlang and continue
   with another request */
class ConfigException : public std::runtime_error {
public:
	ConfigException(const std::string& msg) : std::runtime_error(msg) {}
};


/**
*  Helper class to automatically free ei_x_buff buffers
*/
class XBuffFree
{
public:
    ~XBuffFree() {
        for(std::vector<ei_x_buff*>::reverse_iterator it = list.rbegin(); it != list.rend(); ++it) {
            ei_x_free(*it);
        }
    }

    void add(ei_x_buff* x) {
        list.push_back(x);
    }

private:
    std::vector<ei_x_buff*> list;
};


/**
    Class to manage the stream with Erlang
*/
class EInterface {
public:
    EInterface(xconfig::XConfig& xconf) : xconf(xconf) {}

    bool read_command_from_stdin(void);
    void send_full_config_to_stdout(void);
    void send_close_to_stdout(void);

    /*
        Encode and decode timestamp from/to Erlang.
        The format in Erlang is { tv_sec, tv_nsec }
    */
    static void encode_timestamp(ei_x_buff* x, const struct timespec& tm);

private:

    /* Blocking read and write functions that won't
       return until all the data is read/written, the
       stream is closed or there is any error
    */
    static ssize_t read_full(int fd, void* buffer, size_t count);
    static ssize_t write_full(int fd, const void* buffer, size_t count);

    /* Write the response to Erlang in stdout */
    static void write_response(ei_x_buff* x);
    static void write_error_response(const std::exception& ex);

    void convert_xconfig_to_erlang(ei_x_buff* x);
    void convert(ei_x_buff* x, const XConfigNode& node);

    xconfig::XConfig& xconf;
};


class Reloader {
public:
    Reloader(EInterface& einterface) :
                    update_available(false), should_exit(false), einterface(einterface) {}

    void finish(void);
    void notify_update(int reason);

    /** Wait in its own thread until it's notified that there is an update available */
    void operator()();

private:
    bool update_available;
    bool should_exit;
    int  callback_reason;
    boost::condition_variable cond;
    boost::mutex mutex;
    EInterface& einterface;
};




#endif