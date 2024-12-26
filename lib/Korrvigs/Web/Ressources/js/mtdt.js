function removeAllChilds(node) {
  while(node.firstChild) {
    node.removeChild(node.firstChild)
  }
}

Array.from(document.getElementsByClassName("mtdt-edit-button")).forEach((button) => {
  const rowId = button.getAttribute("data-mtdt-id")
  button.addEventListener("click", function() {
    let row = document.getElementById(rowId)
    let keyTD = row.cells[0]
    let key = keyTD.textContent
    let valTD = row.cells[1]
    let buttonTD = row.cells[2]
    let mtdtUrl = window.location.pathname + "/metadata?key=" + encodeURI(key)
    fetch(mtdtUrl).then((response) => response.json()).then((val) => {
      let keyInput = document.createElement("input")
      keyInput.setAttribute("class", "mtdt-input")
      keyInput.setAttribute("type", "text")
      keyInput.setAttribute("value", key)
      removeAllChilds(keyTD)
      keyTD.appendChild(keyInput)

      let valInput = document.createElement("input")
      valInput.setAttribute("class", "mtdt-input")
      valInput.setAttribute("type", "text")
      valInput.setAttribute("value", JSON.stringify(val.value))
      removeAllChilds(valTD)
      valTD.appendChild(valInput)

      let oldButton = buttonTD.firstChild
      let newButton = document.createElement("button")
      newButton.classList.add("mtdt-button")
      newButton.classList.add("mtdt-confirm-button")
      newButton.setAttribute("data-mtdt-id", rowId)
      newButton.textContent = "✓";
      removeAllChilds(buttonTD)
      buttonTD.appendChild(newButton)

      buttonTD.addEventListener("click", function() {
        const nkey = keyInput.value
        const nval = JSON.parse(valInput.value)
        removeAllChilds(keyTD)
        keyTD.textContent = nkey
        removeAllChilds(valTD)
        valTD.textContent = nval
        removeAllChilds(buttonTD)
        buttonTD.appendChild(oldButton)
        fetch(window.location.pathname + "/metadata", {
          method: "POST",
          body: JSON.stringify({ insert: { [nkey]: nval }, remove: [key] }),
          headers: {
            "Content-Type": "application/json; charset=utf-8"
          }
        })
      })
    })
  })
})
