
#include <string>
#include <memory.h>
#include <stdio.h>

#include "ErlangIO.h"
#include "SnffrLog.h"

int ErlangRX::read_stream() {

  if( m_len < STDIN_STREAM_BUFFER_SIZE )
  {
    int nbytes = read(0, m_buf + m_len, STDIN_STREAM_BUFFER_SIZE - m_len);
    if(nbytes == -1) 
    {   
      LOG("read_stream(): STDIN stream error: errno=%d [%s]\n", errno, strerror(errno));
      return -1;
    }   

    if(nbytes == 0)
    {   
      LOG("read_stream(): STDIN stream closed\n");
      return -1;
    }   

    m_len += nbytes;
  }
  else
  {
    LOG("read_stream(): STDIN stream buffer is full. Cannot read new commands\n");
  }

  return m_len;
}

int ErlangTX::write_stream() {

  int nbytes = write(1, m_buf, m_len);
  if(nbytes == -1)
  {
    LOG("write_stream(): STDOUT stream error: errno=%d [%s]\n", errno, strerror(errno));
    return -1;
  }

  if(nbytes < m_len)  
  {
    LOG("write_stream(): STDOUT stream is full\n");

    int nrest = m_len - nbytes;

    memmove(m_buf, m_buf + nbytes, nrest);

    m_len = nrest;
  }
  else
  {
    m_len  = 0;
  }

  return nbytes;
}


int ErlangRX::read_command(char *buf, int buflen, std::string& cmd, std::vector<std::string>& args) {
  if(m_len < 2) return 0; //not enough data
  
  int cmdlen = ((unsigned char)m_buf[0] << 8) | (unsigned char)m_buf[1];

  if((cmdlen < 0) || (cmdlen > buflen))   
  {
    LOG("read_command(): length %d is invalid or exceeds the buffer size %d\n", cmdlen, buflen);

    memset(m_buf, 0, STDIN_STREAM_BUFFER_SIZE);
    return -1;
  }

  if(cmdlen + 2 > m_len) return 0; //more data to grab on the next read

  memset(buf, 0, buflen);
  memcpy(buf, m_buf + 2, cmdlen);
  memmove(m_buf, m_buf + cmdlen + 2, m_len - cmdlen - 2);

  m_len -= (cmdlen + 2);

  int i = 0, arity = 0;
  if (-1 == ei_decode_version(buf, &i, &arity)) {
    LOG("read_command(): wrong libei versions");
    return -1;
  }
  
  ei_decode_tuple_header(buf, &i, &arity);
  ei_decode_tuple_header(buf, &i, &arity);

  memset((void *)&m_pid, 0, sizeof(erlang_pid));
  ei_decode_pid(buf, &i, &m_pid);

  memset((void *)&m_ref, 0, sizeof(erlang_ref));
  ei_decode_ref(buf, &i, &m_ref);

  ei_decode_tuple_header(buf, &i, &arity);
  char cmd_name[MAXATOMLEN];
  ei_decode_atom(buf, &i, cmd_name); 
  cmd = std::string(cmd_name);
  arity = 0;
  ei_decode_list_header(buf, &i, &arity);
  for (auto arg = 0; arg < arity; arg++) {
    char arg_name[MAXATOMLEN];
    ei_decode_atom(buf, &i, arg_name); 
    args.push_back(arg_name);
  }
 
  return cmdlen;
}

int ErlangTX::write_response(ei_x_buff *buf) {

  if(m_len + buf->index + 2 > STDOUT_STREAM_BUFFER_SIZE)
  {
    LOG("write_response(): Output Stream buffer is full, discarded\n");

    return -1;
  }

  // length
  unsigned char tmp = (unsigned char)((buf->index >> 8) & 0xff);
  m_buf[m_len++] = tmp;

  tmp = (unsigned char)(buf->index & 0xff);
  m_buf[m_len++] = tmp;

  // buffer
  memcpy(m_buf + m_len, buf->buff, buf->index);
  m_len += buf->index;

  return write_stream();
}

int ErlangTX::encode_hdr(ei_x_buff& buf) {

  ei_x_new_with_version(&buf);

  ei_x_encode_tuple_header(&buf, 2);

  ei_x_encode_tuple_header(&buf, 2);
  ei_x_encode_pid(&buf, &m_pid);
  ei_x_encode_ref(&buf, &m_ref);

  return 0;
}

// {ok, [binary()]}
int ErlangTX::reply_ok( const std::vector<std::string>& reply) {
  ei_x_buff buf;
  encode_hdr(buf);
  ei_x_encode_tuple_header(&buf, 2);
  ei_x_encode_atom(&buf, "ok");
  ei_x_encode_list_header(&buf, reply.size());
  for (auto bin: reply) {
    ei_x_encode_binary(&buf, bin.c_str(), bin.length());
  }
  ei_x_encode_empty_list(&buf);
  int rv = write_response(&buf);

  ei_x_free(&buf);
  return rv;
}

// {error, Reason}
int ErlangTX::reply_error(const std::string& msg) {
  ei_x_buff buf;

  encode_hdr(buf);
  ei_x_encode_tuple_header(&buf, 2);
  ei_x_encode_atom(&buf, "error");
  ei_x_encode_atom(&buf, msg.c_str());

  int rv = write_response(&buf);

  ei_x_free(&buf);
  return rv;

}


