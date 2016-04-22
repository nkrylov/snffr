// raw IP packets capture Erlang interfac// raw IP packets capture Erlang interfacee


#include <iostream>
#include <pcap.h>
#include <memory.h>
#include <stdlib.h>

#include "SnffrLog.h"
#include "ErlangIO.h"
#include "Snffr.h"

#include "ei.h"

const unsigned int TMO_DEFAULT = 1000;

int main( int argc, char *argv[] ) {

  char cmd_buf[MAX_ERL_PIPE_SZ];
  ErlangRX rx;
  ErlangTX tx;

  char* dev;
  char errbuf[PCAP_ERRBUF_SIZE];
  pcap_t* descr = NULL;
  const u_char *packet;
  struct pcap_pkthdr hdr;

  const unsigned int pcap_tmo = (argc > 1) ? atoi(argv[1]) : TMO_DEFAULT;

  while(1) {

    if (-1 == rx.read_stream()) break;

    unsigned int cmd_len = 0;
    std::string cmd;
    erlang_pid pid;
    erlang_ref ref;
 
    while( (cmd_len = rx.read_command(cmd_buf, MAX_ERL_PIPE_SZ, pid, ref, cmd)) > 0 ) {
      
      // execute

      if (0 == cmd.compare("attach")) {

          if (NULL != descr) {
            tx.reply_error(pid, ref, "already_opened");
          }

          dev = pcap_lookupdev(errbuf);

          if (NULL == dev) {
            tx.reply_error(pid, ref, errbuf);
            continue;
          } 

          descr = pcap_open_live(dev, BUFSIZ, 1, pcap_tmo, errbuf);
          
          if (NULL == descr) {
            tx.reply_error(pid, ref, errbuf);
            continue;
          }

          tx.reply_ok(pid, ref, dev, strlen(dev));      

      } else if (0 == cmd.compare("get_packet")) {

        if (NULL == descr) {
          tx.reply_error(pid, ref, "not_initialized");    
          continue;
        }
        
        packet = pcap_next(descr,&hdr);
        if (NULL == packet) {
          tx.reply_error(pid, ref, "timeout");  
          continue;
        }

        tx.reply_ok(pid, ref, (char *)packet, hdr.caplen);
      } else if (0 == cmd.compare("detach")) {

        if (NULL == descr) {
          tx.reply_error(pid, ref, "not_initialized");    
          continue;
        }

        pcap_close(descr);
        descr = NULL;

        tx.reply_ok(pid, ref, dev, strlen(dev));
        dev = NULL;

      } else {  
        tx.reply_error(pid, ref, "unknown_command");
      }
   
    }

  }  
  return 0;
}
