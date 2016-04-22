// Buffered i/o between C++ app and Erlang VM
#include "ei.h"
#include <unistd.h>

const unsigned long STDIN_STREAM_BUFFER_SIZE = 1024;
const unsigned long STDOUT_STREAM_BUFFER_SIZE = 1024*1024;
const unsigned int  ERL_PACKET_SZ = 16; 

class ErlangIO {

protected:
  int m_dscr;
  
public:
  ErlangIO(int fd) : m_dscr(fd) {}
  virtual ~ErlangIO() {}
};

class ErlangRX : public ErlangIO {

private:
    char m_buf[STDIN_STREAM_BUFFER_SIZE];
    int m_len;
 
public:
    ErlangRX() : ErlangIO(0), m_len(0) {}
    virtual ~ErlangRX() {}
    int read_command(char *buf, int buflen, 
      erlang_pid& pid, erlang_ref& ref, std::string& cmd);
    int read_stream();
 
};

class ErlangTX : public ErlangIO {

private:
    char m_buf[STDOUT_STREAM_BUFFER_SIZE];
    int m_len;
    int write_stream();
    int write_response(ei_x_buff *buff);
   
public:
    ErlangTX() : ErlangIO(1), m_len(0) {}
    virtual ~ErlangTX() { } 
    int reply_ok(erlang_pid pid, erlang_ref ref, char *result_buf, unsigned int len); 
    int reply_error(erlang_pid pid, erlang_ref ref, const std::string& msg);
};

