namespace owl {
  template<typename Payload>
  class Bridge {
  public:
    Bridge(Payload (* callback)(Payload)) {
      event_callback = callback;
    }

    // Calls event_callback and passes back its return value to the
    // caller.  This is how Swift triggers an event in the UI.
    Payload trigger_event(Payload payload) {
      return event_callback(payload);
    }
    
  private:
    Payload (* event_callback)(Payload);
  };
}
