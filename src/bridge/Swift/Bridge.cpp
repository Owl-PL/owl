#include "Bridge.hh"

using namespace owl;

int my_event_callback(int p) {
  return p + 5;
}

Bridge<int> bridge(&my_event_callback);


