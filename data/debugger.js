window.onload = function () {
  const eventSource = new EventSource("events");
  window.onbeforeunload = () => eventSource.close();

  eventSource.addEventListener("started", handleEmulatorStarted);
  eventSource.addEventListener("paused", handleEmulatorPaused);
  eventSource.addEventListener("status", handleStatusUpdate);
};

let status = "";

function handleEmulatorStarted(event) {
  const disableButton = button => button.setAttribute("disabled", true);

  if (status !== "started") {
    status = "started";
    document.getElementsByName('run')
      .forEach(element => element.innerText = "Pause");
    document.getElementsByName('step').forEach(disableButton);
    document.getElementsByName('stepOver').forEach(disableButton);
    document.getElementsByName('stepOut').forEach(disableButton);
  }
}

function handleEmulatorPaused(event) {
  const enableButton = button => button.removeAttribute("disabled");

  if (status !== "paused") {
    status = "paused";
    document.getElementsByName('run')
      .forEach(element => element.innerText = "Run");
    document.getElementsByName('step').forEach(enableButton);
    document.getElementsByName('stepOver').forEach(enableButton);
    document.getElementsByName('stepOut').forEach(enableButton);
  }

}

function handleStatusUpdate(event) {
  const data = JSON.parse(event.data);
  for (let key of Object.keys(data)) {
    document.getElementById(key).innerText = data[key];
  }
}
