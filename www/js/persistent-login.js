// Persistent login bridge: stores a remember-me token in localStorage and
// relays it to the Shiny "auth" module. Only a random token is ever stored
// client-side — never a password or password hash.
//
// This is strictly optional: if anything here fails or never runs, the app
// must remain fully usable (the user can still sign in manually). Every path is
// wrapped so a failure can never interfere with Shiny's own startup.

(function () {
  "use strict";
  try {
    if (typeof Shiny === "undefined" || !Shiny.addCustomMessageHandler) {
      return;
    }

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

    Shiny.addCustomMessageHandler("bw_storeUser", function (data) {
      if (!available()) return;
      try {
        var s = typeof data === "string" ? data : JSON.stringify(data);
        localStorage.setItem(KEY, s);
      } catch (e) {}
    });

    Shiny.addCustomMessageHandler("bw_clearStoredUser", function () {
      if (!available()) return;
      try {
        localStorage.removeItem(KEY);
      } catch (e) {}
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

    // Announce readiness once the Shiny session is connected. This file is
    // included after Shiny's JS in the rendered page, so the handlers above are
    // already registered by the time this fires.
    $(document).on("shiny:connected", function () {
      try {
        Shiny.setInputValue("auth-jsReady", Date.now(), { priority: "event" });
      } catch (e) {}
    });
  } catch (e) {
    if (window.console) console.warn("persistent-login init skipped:", e);
  }
})();
