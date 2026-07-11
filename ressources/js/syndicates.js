document.addEventListener("DOMContentLoaded", function () {
  const buttons = document.getElementsByClassName("item-read");
  for(const button of buttons) {
    button.addEventListener("click", () => {
      const readUrl = button.getAttribute("item-read-url");
      fetch(readUrl).then(() => {
        const liId = button.getAttribute("item-to-read");
        document.getElementById(liId).classList.add("read");
        button.remove();
      });
    });
  }
});
