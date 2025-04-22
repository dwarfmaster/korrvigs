function setupActions() {
  const actions = document.getElementsByClassName("action")
  const target = document.getElementById("actions-form-container")
  for (const action of actions) {
    const formId = action.getAttribute("data-action-form")
    action.addEventListener("click", function () {
      const form = document.getElementById(formId).firstChild.cloneNode(true)
      target.innerHTML = ''
      target.appendChild(form)

      form.addEventListener("submit", (ev) => {
        ev.preventDefault()
        const dat = new FormData(form)
        fetch(form.getAttribute("action"), {
          method: "POST",
          body: dat
        }).then((response) => response.json()).then((react) => {
            if('redirect' in react) {
              window.location = react.redirect
            }
            target.innerHTML = ''
            if('message' in react) {
              target.innerHTML = react.message
            } else {
              target.innerHTML = "<p>Success</p>"
            }
            if('alert' in react) {
              alert(react.alert)
            }
        })
      })
    })
  }
}
