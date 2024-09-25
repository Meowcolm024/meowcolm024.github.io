// go "up" function

const url = window.location.href;
if (url.endsWith('html')) {
  document
    .getElementById("back")
    .setAttribute("href", url.split('/').slice(0, -1).join('/'));
}
