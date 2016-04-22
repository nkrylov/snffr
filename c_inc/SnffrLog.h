#ifndef __SNFFR_LOG_H__
#define __SNFFR_LOG_H__

#include <stdio.h>

#define LOG(msg, args...) { fprintf(stderr, msg, ##args); }

#endif //__SNFFR_LOG_H__
