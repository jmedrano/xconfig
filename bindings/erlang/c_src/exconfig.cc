/* The Erlang Interface library use the type "long long".
   This is to avoid the warning:
   "warning: ISO C++ 1998 does not support 'long long'"
   when compiling in pedantic mode */
#pragma GCC diagnostic ignored "-Wlong-long"

#include <iostream>
#include <boost/thread/thread.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/tokenizer.hpp>
#include <boost/algorithm/string.hpp>
#include <unistd.h>
#include <set>

#include "exconfig.h"

using xconfig::XConfigValueType;


void EInterface::convert_xconfig_to_erlang(ei_x_buff* x) {

    XConfigNode node = xconf.getNode("");

    if (node.getType() != xconfig::TYPE_MAP) {
        throw ConfigException("The top level must be a map");
    }

    convert(x, node);
}


void EInterface::convert(ei_x_buff* x, const XConfigNode& node) {

    switch (node.getType()) {
        case xconfig::TYPE_MAP:
            {
                const std::vector<XConfigNode> children = node.getChildren();

                /* XConfig can return duplicated keys. We want to get rid of them and only use the latest one.
                   We use a map to store the key and its last index */
                std::map<std::string, size_t> keys;

                size_t i = 0;
                for(std::vector<XConfigNode>::const_iterator it = children.begin(); it != children.end(); ++it, ++i) {
                    const XConfigNode& child = *it;
                    keys[child.getName()] = i;
                }

                // Creates a set with the valid indexes
                std::set<size_t> indexes;
                for(std::map<std::string, size_t>::const_iterator it = keys.begin(); it != keys. end(); ++it) {
                    indexes.insert(it->second);
                }

                ei_x_encode_map_header(x, indexes.size());

                i = 0;
                for(std::vector<XConfigNode>::const_iterator it = children.begin(); it != children.end(); ++it, ++i) {
                    // Ignore invalid indexes
                    if (indexes.find(i) == indexes.end())
                        continue;

                    const XConfigNode& child = *it;

                    // The first element will be the key
                    ei_x_encode_atom(x, child.getName().c_str());
                    // The second one the value
                    convert(x, child);
                }
            }
            break;

        case xconfig::TYPE_SEQUENCE:
            {
                const std::vector<XConfigNode> children = node.getChildren();

                if (children.size() > 0) {
                    ei_x_encode_list_header(x, children.size());
                }
                for(std::vector<XConfigNode>::const_iterator it = children.begin(); it != children.end(); ++it) {
                    convert(x, *it);
                }
                ei_x_encode_empty_list(x);
            }
            break;

        case xconfig::TYPE_STRING:
            {
                const std::string s = node.getString();
                ei_x_encode_binary(x, s.data(), s.length());
            }
            break;

        case xconfig::TYPE_INTEGER:
            ei_x_encode_longlong(x, static_cast<long long>(node.getInt()));
            break;

        case xconfig::TYPE_FLOAT:
            ei_x_encode_double(x, static_cast<double>(node.getFloat()));
            break;

        case xconfig::TYPE_BOOLEAN:
            ei_x_encode_boolean(x, static_cast<int>(node.getBool()));
            break;

        case xconfig::TYPE_NULL:
        default:
            ei_x_encode_atom(x, "null");
    }
}

ssize_t EInterface::read_full(int fd, void* b, size_t count) {
    uint8_t* buffer = static_cast<uint8_t*>(b);
    size_t bytes_read = 0;

    while(bytes_read < count) {
        ssize_t res = read(fd, buffer, count - bytes_read);
        if (res == 0) {
            // End of file
            return bytes_read;
        }

        if (res < 0) {
            // Error
            return res;
        }

        bytes_read += res;
        buffer += res;
    }

    return bytes_read;
}

ssize_t EInterface::write_full(int fd, const void* b, size_t count) {
    const uint8_t* buffer = static_cast<const uint8_t*>(b);
    size_t bytes_written = 0;

    while (bytes_written < count) {
        ssize_t res = write(fd, buffer, count - bytes_written);
        if (res == 0) {
            // Stream closed?
            return bytes_written;
        }

        if (res < 0) {
            // Error
            return res;
        }

        bytes_written += res;
        buffer += res;
    }

    return bytes_written;
}

void EInterface::write_response(ei_x_buff* x) {
    /* Some books use x->buffsz as the packet length, but that is the total size of the
       reserved buffer for ei_x_buff (reserved but may be not initialized).
       So the correct length to use it's the index
    */
    packet_size_t packet_len = packet_length_swap_out(static_cast<packet_size_t>(x->index));
    if (write_full(1, &packet_len, sizeof(packet_size_t)) != sizeof(packet_size_t)) {
        throw StreamException("Can't write response to Erlang");
    }

    if (write_full(1, x->buff, x->index) != x->index) {
        throw StreamException("Can't write response to Erlang");
    }
}


bool EInterface::read_command_from_stdin(void) {
    /* We don't accept commands, so if we receive any data from Erlang
       we discard it
    */
    packet_size_t packet_len = 0;
    int n;

    // Read the packet length from stdin
    n = read_full(0, &packet_len, sizeof(packet_size_t));
    if (n == 0) {
        // stdin closed
        return false;
    }

    if (n < static_cast<int>(sizeof(packet_size_t))) {
        throw StreamException("Can't read packet size");
    }

    // The packet length is always in big endian format
    packet_len = packet_length_swap_in(packet_len);

    if (packet_len > MAX_PACKET_SIZE) {
        throw StreamException("Bad packet size");
    }
    std::vector<char> buffer;
    buffer.resize(packet_len);

    n = read_full(0, buffer.data(), packet_len);
    if (n != static_cast<int>(packet_len)) {
        throw StreamException("Can't discard the command");
    }

    return true;
}

void EInterface::send_full_config_to_stdout(void) {
    /* Send {config, NewConfig} */
    XBuffFree xbuff_free;

    try {
        ei_x_buff x_full_config;
        if (ei_x_new_with_version(&x_full_config) != 0) {
            throw ConfigException("Can't create response for new configuration");
        }
        xbuff_free.add(&x_full_config);

        ei_x_encode_tuple_header(&x_full_config, 2);
        ei_x_encode_atom(&x_full_config, "config");

        convert_xconfig_to_erlang(&x_full_config);

        write_response(&x_full_config);
    }
    catch (const ConfigException& ex) {
        write_error_response(ex);
    }
    catch (const xconfig::XConfigException& ex) {
        write_error_response(ex);
    }
}

void EInterface::send_close_to_stdout(void) {
    /* Send the atom 'close' */
    XBuffFree xbuff_free;

    ei_x_buff x_close;
    if(ei_x_new_with_version(&x_close) != 0) {
        throw ConfigException("Can't create command for connection close");
    }
    xbuff_free.add(&x_close);

    ei_x_encode_atom(&x_close, "close");

    write_response(&x_close);
}

void EInterface::write_error_response(const std::exception& ex) {
    /* Generate the response
       {config, {error, Reason}}
    */

    XBuffFree xbuff_free;
    ei_x_buff x;
    if (ei_x_new_with_version(&x) != 0) {
        // We can't do anything in this case
        throw ex;
    }

    xbuff_free.add(&x);
    ei_x_encode_tuple_header(&x, 2);
    ei_x_encode_atom(&x, "config");
    ei_x_encode_tuple_header(&x, 2);
    ei_x_encode_atom(&x, "error");
    ei_x_encode_string(&x, ex.what());

    write_response(&x);
}


static void print_usage(const std::string& cmd) {
    std::cerr << "Usage:" << std::endl;
    std::cerr << cmd << " config_path socket_path" << std::endl;
}


void Reloader::finish(void) {
    {
        boost::lock_guard<boost::mutex> lock(mutex);
        should_exit = true;
    }

    cond.notify_one();
}

void Reloader::notify_update(int reason) {
    {
        boost::lock_guard<boost::mutex> lock(mutex);
        update_available = true;
        callback_reason = reason;
    }

    cond.notify_one();
}

void Reloader::operator()() {
    boost::unique_lock<boost::mutex> lock(mutex);
    do {
        while(!update_available && !should_exit)
        {
            cond.wait(lock);
        }

        if (update_available) {
            switch(callback_reason) {
                case xconfig::UnixConnectionPool::CALLBACK_RELOAD:
                    einterface.send_full_config_to_stdout();
                    break;

                case xconfig::UnixConnectionPool::CALLBACK_CLOSE:
                    einterface.send_close_to_stdout();
                    // We terminate the program now
                    exit(1);
                    break;

                // Unrecognized reasons are ignored
                default:
                    break;
            }
            update_available = false;
        }

    } while (!should_exit);
}

void updateCallback(int reason, void* extra, void* ptr) {
    Reloader* reloader = static_cast<Reloader*>(ptr);
    /* Just notify to the reloader in its own thread
       to send a message to Erlang.
       While we are in this callback we are blocking
       the XConfig update thread
    */
    reloader->notify_update(reason);
}


int main(int argc, char** argv) {

    if (argc < 3) {
        std::cerr << "Error: Insufficient parameters" << std::endl << std::endl;
        print_usage(argv[0]);
        return 1;
    }
#ifdef HAS_EI_INIT
    if (ei_init() != 0) {
        std::cerr << "Error: Can't initialize ei library" << std::endl << std::endl;
        return 2;
    }
#else
    erl_init(NULL, 0);
#endif

    xconfig::UnixConnectionPool pool(xconfig::UnixConnectionPool::DEFAULT_TIMEOUT, true);
    boost::shared_ptr<xconfig::LinkedConnection> conn;

    int number_of_tries = 0;
    while(1) {
        try {
            conn = pool.getConnection(argv[1], argv[2]);
            break;
        }
        catch (const xconfig::XConfigNotConnected& ex) {
            if (++number_of_tries > CONNECTION_TIMEOUT) {
                throw(ex);
            }
            else {
                sleep(1);
            }
        }
    }

    xconfig::XConfig config = xconfig::XConfig(conn, true);

    EInterface interface(config);
    Reloader reloader(interface);

    // Force the initial update
    reloader.notify_update(xconfig::UnixConnectionPool::CALLBACK_RELOAD);
    pool.setCallbackInfo(updateCallback, &reloader);

    boost::thread reloader_thread(boost::ref(reloader));

    try {

        while (interface.read_command_from_stdin()) {
            /* We don't accept any command,
               but we discard data until
               stdin is closed
            */
        }
    }
    catch (const std::exception& ex) {
        std::cerr << "Exception: " << ex.what() << std::endl;
        return 1;
    }

    reloader.finish();
    reloader_thread.join();


    return 0;
}
