(function () {
    "use strict";

    var algoSelect = document.getElementById("algorithm");
    var implSelect = document.getElementById("implementation");
    var platSelect = document.getElementById("platform");
    var inputText = document.getElementById("input-text");
    var computeBtn = document.getElementById("compute-btn");
    var loadingEl = document.getElementById("loading");
    var errorEl = document.getElementById("error");
    var outputEl = document.getElementById("output");
    var outSelection = document.getElementById("out-selection");
    var outInput = document.getElementById("out-input");
    var outHash = document.getElementById("out-hash");
    var outTime = document.getElementById("out-time");

    // Platform availability per implementation
    var platformSupport = {
        c: ["native", "linux-arm64", "android-arm64"],
        cpp: ["native"],
        "erlang-nif": ["native"],
        "erlang-pure": ["native"]
    };

    function updatePlatformOptions() {
        var impl = implSelect.value;
        var supported = platformSupport[impl] || ["native"];
        var options = platSelect.options;

        for (var i = 0; i < options.length; i++) {
            var opt = options[i];
            if (supported.indexOf(opt.value) === -1) {
                opt.disabled = true;
                if (opt.selected) {
                    platSelect.value = "native";
                }
            } else {
                opt.disabled = false;
            }
        }
    }

    implSelect.addEventListener("change", updatePlatformOptions);
    updatePlatformOptions();

    computeBtn.addEventListener("click", function () {
        var algo = algoSelect.value;
        var impl = implSelect.value;
        var plat = platSelect.value;
        var input = inputText.value;

        errorEl.classList.add("hidden");
        outputEl.classList.add("hidden");
        loadingEl.classList.remove("hidden");
        computeBtn.disabled = true;

        fetch("/api/hash", {
            method: "POST",
            headers: {"Content-Type": "application/json"},
            body: JSON.stringify({
                algorithm: algo,
                implementation: impl,
                platform: plat,
                input: input
            })
        })
        .then(function (res) { return res.json(); })
        .then(function (data) {
            loadingEl.classList.add("hidden");
            computeBtn.disabled = false;

            if (data.error) {
                errorEl.textContent = data.error;
                errorEl.classList.remove("hidden");
                return;
            }

            outSelection.textContent = data.algorithm.toUpperCase() +
                " / " + data.implementation +
                " / " + data.platform;
            outInput.textContent = data.input;
            outHash.textContent = data.hash;
            outTime.textContent = data.exec_time_ms.toFixed(3) + " ms";
            outputEl.classList.remove("hidden");
        })
        .catch(function (err) {
            loadingEl.classList.add("hidden");
            computeBtn.disabled = false;
            errorEl.textContent = "Request failed: " + err.message;
            errorEl.classList.remove("hidden");
        });
    });
})();
