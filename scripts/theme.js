// light/dark mode support

function init(pd) {
    let theme = localStorage.getItem("theme");
    if (!theme) {
        // we can auto detect browser dark mode
        // https://stackoverflow.com/a/57795495/10499685
        if (pd && window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
            theme = "dark";
        } else {
            theme = "light";
        }
    }
    set_theme(theme);
}

function dark_mode() {
    let theme = localStorage.getItem("theme");
    if (!theme) { return init(); }
    set_theme(theme === "light" ? "dark" : "light");
}

function set_theme(theme) {
    document
        .querySelector("html")
        .setAttribute("theme", theme);
    localStorage.setItem("theme", theme);
    document.getElementById("dark").textContent = theme;
    if (theme === "light") {
        document.getElementById("prism").href = "/css/prism-vs.css";
    } else {
        document.getElementById("prism").href = "/css/prism-vsc-dark-plus.css";
    }
}

init(false); // disable browser dark mode detection
