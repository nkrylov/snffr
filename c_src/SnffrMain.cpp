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
  pcap_t* descr = NULL;
  pcap_if_t *interfaces = NULL,*temp;
  char errbuf[PCAP_ERRBUF_SIZE];
  const u_char *packet;
  struct pcap_pkthdr hdr;

  const unsigned int pcap_tmo = (argc > 1) ? atoi(argv[1]) : TMO_DEFAULT;

  while(1) {

    if (-1 == rx.read_stream()) break;

    unsigned int cmd_len = 0;
    std::string cmd;
    std::vector<std::string> args;
 
    while( (cmd_len = rx.read_command(cmd_buf, MAX_ERL_PIPE_SZ, cmd, args)) > 0 ) {
      
      // execute
      ErlangTX tx(rx);
      if (0 == cmd.compare("init")) {

        if (-1 == pcap_findalldevs(&interfaces, errbuf)) {
          tx.reply_error(errbuf);
          continue;
        }

        std::vector<std::string> devs; 
        for(temp=interfaces;temp;temp=temp->next)
        {
            devs.push_back(std::string(temp->name));      
        }
        tx.reply_ok(devs);
      } else if (0 == cmd.compare("attach") && (1 == args.size())) {

          if (NULL != descr) {
            tx.reply_error("already_opened");
          }

          const char *dev = args[0].c_str();
          fprintf(stderr, "attching to %s\n", args[0].c_str());
          descr = pcap_open_live(dev, BUFSIZ, 1, pcap_tmo, errbuf);
          
          if (NULL == descr) {
            tx.reply_error(errbuf);
            continue;
          }
          std::vector<std::string> reply;
          reply.push_back(dev);
          tx.reply_ok(reply);      

      } else if (0 == cmd.compare("get_packet")) {

        if (NULL == descr) {
          tx.reply_error("not_initialized");    
          continue;
        }
        
        packet = pcap_next(descr,&hdr);
        if (NULL == packet) {
          tx.reply_error("timeout");  
          continue;
        }
        std::string bin((char*)packet, hdr.caplen);
        std::vector<std::string> reply;
        reply.push_back(bin);
        tx.reply_ok(reply);
      } else if (0 == cmd.compare("shutdown")) {

        if (NULL == descr) {
          tx.reply_error("not_initialized");    
          continue;
        }

        pcap_close(descr);
        descr = NULL;

        tx.reply_ok(std::vector<std::string>());
        descr = NULL;

      } else {  
        tx.reply_error("unknown_command");
      }
   
    }

  }  
  return 0;
}
