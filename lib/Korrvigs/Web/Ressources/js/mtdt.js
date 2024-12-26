function removeAllChilds(node) {
  while(node.firstChild) {
    node.removeChild(node.firstChild)
  }
}

function mtdtSetupConfirmButton(newButton, oldButton, key, keyInput, valInput) {
  newButton.addEventListener("click", function() {
    const nkey = keyInput.value
    const nval = JSON.parse(valInput.value)
    let keyTD = keyInput.parentElement
    removeAllChilds(keyTD)
    keyTD.textContent = nkey
    let valTD = valInput.parentElement
    removeAllChilds(valTD)
    valTD.textContent = JSON.stringify(nval)
    const buttonTD = newButton.parentElement
    removeAllChilds(buttonTD)
    buttonTD.appendChild(oldButton)
    let toRemove = key ? [key] : []
    fetch(window.location.pathname + "/metadata", {
      method: "POST",
      body: JSON.stringify({ insert: { [nkey]: nval }, remove: toRemove }),
      headers: {
        "Content-Type": "application/json; charset=utf-8"
      }
    })
  })
}

function mtdtSetupEditButton(button) {
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
      newButton.textContent = "✓"
      removeAllChilds(buttonTD)
      buttonTD.appendChild(newButton)

      mtdtSetupConfirmButton(newButton, oldButton, key, keyInput, valInput)
    })
  })
}

Array.from(document.getElementsByClassName("mtdt-edit-button")).forEach(mtdtSetupEditButton)

function mtdtSetupRmButton(button) {
  const rowId = button.getAttribute("data-mtdt-id")
  button.addEventListener("click", function() {
    let row = document.getElementById(rowId)
    let keyTD = row.cells[0]
    let key = keyTD.textContent
    row.remove()
    fetch(window.location.pathname + "/metadata", {
      method: "POST",
      body: JSON.stringify({ insert: {}, remove: [key] }),
      headers: {
        "Content-Type": "application/json; charset=utf-8"
      }
    })
  })
}

Array.from(document.getElementsByClassName("mtdt-rm-button")).forEach(mtdtSetupRmButton)

function mtdtAddButtonSetup() {
  let addButton = document.getElementById("mtdt-add-button")
  let addTr = addButton.parentElement.parentElement
  addButton.addEventListener("click", function() {
    let uid = Date.now().toString(36) + Math.random().toString(36).substring(2,12)
    let tr = document.createElement("tr")
    tr.id = uid

    let tdKey = document.createElement("td")
    tdKey.classList.add("mtdt-key")
    let keyInput = document.createElement("input")
    keyInput.setAttribute("class", "mtdt-input")
    keyInput.setAttribute("type", "text")
    tdKey.appendChild(keyInput)
    tr.appendChild(tdKey)

    let tdVal = document.createElement("td")
    tdVal.classList.add("mtdt-value")
    let valInput = document.createElement("input")
    valInput.setAttribute("class", "mtdt-input")
    valInput.setAttribute("type", "text")
    tdVal.appendChild(valInput)
    tr.appendChild(tdVal)

    let editButton = document.createElement("button")
    editButton.classList.add("mtdt-button")
    editButton.classList.add("mtdt-edit-button")
    editButton.setAttribute("data-mtdt-id", uid)
    editButton.textContent = "✎"
    mtdtSetupEditButton(editButton)

    let tdEdit = document.createElement("td")
    tdEdit.classList.add("mtdt-button-case")
    let confirmButton = document.createElement("button")
    confirmButton.classList.add("mtdt-button")
    confirmButton.classList.add("mtdt-confirm-button")
    confirmButton.setAttribute("data-mtdt-id", uid)
    confirmButton.textContent = "✓"
    tdEdit.appendChild(confirmButton)
    mtdtSetupConfirmButton(confirmButton, editButton, null, keyInput, valInput)
    tr.appendChild(tdEdit)

    let tdRm = document.createElement("td")
    tdRm.classList.add("mtdt-button-case")
    let rmButton = document.createElement("button")
    rmButton.classList.add("mtdt-button")
    rmButton.classList.add("mtdt-rm-button")
    rmButton.setAttribute("data-mtdt-id", uid)
    rmButton.textContent = "❌"
    mtdtSetupRmButton(rmButton)
    tdRm.appendChild(rmButton)
    tr.appendChild(tdRm)

    addTr.parentElement.insertBefore(tr, addTr)
  })
}
mtdtAddButtonSetup()
