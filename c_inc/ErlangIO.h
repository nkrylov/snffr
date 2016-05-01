// Buffered i/o between C++ app and Erlang VM

#include <string>
#include <vector>

#include "ei.h"
#include <unistd.h>


const unsigned long STDIN_STREAM_BUFFER_SIZE = 1024;
const unsigned long STDOUT_STREAM_BUFFER_SIZE = 1024*1024;
const unsigned int  ERL_PACKET_SZ = 16; 


class ErlangIO {

protected:
  erlang_pid m_pid;
  erlang_ref m_ref;
  int m_dscr;
  
public:
  const erlang_pid get_pid() {return m_pid;}
  const erlang_ref get_ref() {return m_ref;}
  ErlangIO(int fd) : m_dscr(fd) {}
  ErlangIO(int fd, erlang_pid p, erlang_ref r) : m_dscr(fd), m_pid(p), m_ref(r) {}
  virtual ~ErlangIO() {}
};

class ErlangRX : public ErlangIO {

private:
    char m_buf[STDIN_STREAM_BUFFER_SIZE];
    int m_len;
 
public:
    ErlangRX() : ErlangIO(0), m_len(0) {}
    virtual ~ErlangRX() {}
    int read_command(char *buf, int buflen, std::string& cmd, std::vector<std::string>& args);
    int read_stream();
 
};

class ErlangTX : public ErlangIO {

private:
    char m_buf[STDOUT_STREAM_BUFFER_SIZE];
    int m_len;
    int encode_hdr(ei_x_buff& buf);
    int write_stream();
    int write_response(ei_x_buff *buff);
   
public:
//    ErlangTX(ErlangRX& rx) : ErlangIO(1), m_len(0) { set_pid(rx.get_pid()); set_ref(rx.get_ref()); }
    ErlangTX(ErlangRX& rx) : ErlangIO(1, rx.get_pid(), rx.get_ref()), m_len(0) {}
    virtual ~ErlangTX() { } 
    int reply_to(const ErlangRX& rx);
    int reply_ok(const std::vector<std::string>& reply); 
    int reply_error(const std::string& msg);
};

