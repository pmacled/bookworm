// Persistent login bridge: stores a remember-me token in localStorage and
// relays it to the Shiny "auth" module. Only a random token is ever stored
// client-side — never a password or password hash.
//
// This file may be loaded from <head>, i.e. before Shiny's own JS. We therefore
// poll until Shiny exists, register the custom message handlers, and only then
// tell the server we are ready (auth-jsReady). The server waits for that flag
// before sending bw_getStoredUser, so a message can never arrive without a
// handler — an unhandled custom message can otherwise break input binding and
// make the page unclickable.

(function () {
  var KEY = "bookwormUser";
  var INPUT = "auth-storedUser"; // ns("auth") + "storedUser"

  function available() {
    try {
      var t = "__bw_test__";
      localStorage.setItem(t, t);
      localStorage.removeItem(t);
      return true;
    } catch (e) {
      return false;
    }
  }

  function register() {
    Shiny.addCustomMessageHandler("bw_storeUser", function (data) {
      if (!available()) return;
      try {
        var s = typeof data === "string" ? data : JSON.stringify(data);
        localStorage.setItem(KEY, s);
      } catch (e) {
        /* ignore */
      }
    });

    Shiny.addCustomMessageHandler("bw_clearStoredUser", function () {
      if (!available()) return;
      try {
        localStorage.removeItem(KEY);
      } catch (e) {
        /* ignore */
      }
    });

    Shiny.addCustomMessageHandler("bw_getStoredUser", function () {
      var v = "";
      if (available()) {
        try {
          v = localStorage.getItem(KEY) || "";
        } catch (e) {
          v = "";
        }
      }
      Shiny.setInputValue(INPUT, v, { priority: "event" });
    });
  }

  // Once Shiny is connected, register handlers and announce readiness. Using
  // shiny:connected guarantees the server input pipeline is up.
  function onReady() {
    register();
    Shiny.setInputValue("auth-jsReady", Date.now(), { priority: "event" });
  }

  // Poll for Shiny (this script may run before Shiny's library is parsed).
  var tries = 0;
  var timer = setInterval(function () {
    tries += 1;
    if (typeof Shiny !== "undefined" && Shiny.addCustomMessageHandler) {
      clearInterval(timer);
      if (typeof $ !== "undefined") {
        $(document).on("shiny:connected", onReady);
        // If already connected by the time we attached, fire once now.
        if (Shiny.shinyapp && Shiny.shinyapp.$socket) onReady();
      } else {
        onReady();
      }
    } else if (tries > 200) {
      clearInterval(timer); // give up after ~10s; app still works without persist-login
    }
  }, 50);
})();
