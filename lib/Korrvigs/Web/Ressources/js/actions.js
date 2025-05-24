function setupActions(otherForm) {
  const actions = document.getElementsByClassName("action")
  const target = document.getElementById("actions-form-container")
  for (const action of actions) {
    const formId = action.getAttribute("data-action-form")
    action.addEventListener("click", function () {
      const oldForm = target.getAttribute("data-action-form")
      if(oldForm) {
        const oldTemplate = document.getElementById(oldForm)
        const oldFormEl = target.firstChild
        target.innerHTML = ''
        oldTemplate.appendChild(oldFormEl)
      }

      const template = document.getElementById(formId)
      const form = template.firstChild
      template.innerHTML = ''

      target.appendChild(form)
      target.setAttribute("data-action-form", formId)

      form.addEventListener("submit", (ev) => {
        ev.preventDefault()
        const dat = new FormData(form)
        if(otherForm) {
          const otherDat = new FormData(document.getElementById(otherForm))
          otherDat.keys().forEach(key => {
            otherDat.getAll(key).forEach(val => {
              dat.append(key, val)
            })
          })
        }

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
