// Persistent login bridge: stores a remember-me token in localStorage and
// relays it to the Shiny "auth" module. Only a random token is ever stored
// client-side — never a password or password hash.

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

  $(document).on("shiny:connected", function () {
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
  });
})();
