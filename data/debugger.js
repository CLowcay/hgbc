document.addEventListener("DOMContentLoaded", function (event) {
  const eventSource = new EventSource("events");

  eventSource.onmessage = handleDebugEvent;
});

function enableButton(button) {
  button.removeAttribute("disabled");
}

function disableButton(button) {
  button.setAttribute("disabled", true);
}

let status = "";

const handleDebugEvent = function (event) {
  const data = JSON.parse(event.data);
  if (data.status === "started" && data.status !== status) {
    document.getElementsByName('run')
      .forEach(function (element) { element.innerText = "Pause" });
    document.getElementsByName('step').forEach(disableButton);
    document.getElementsByName('stepOver').forEach(disableButton);
    document.getElementsByName('stepOut').forEach(disableButton);
  } else if (data.status == "paused" && data.status !== status) {
    document.getElementsByName('run')
      .forEach(function (element) { element.innerText = "Run" });
    document.getElementsByName('step').forEach(enableButton);
    document.getElementsByName('stepOver').forEach(enableButton);
    document.getElementsByName('stepOut').forEach(enableButton);
  } else {
    console.log("Invalid status " + data.status);
  }
}
