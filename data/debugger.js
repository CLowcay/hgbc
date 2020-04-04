const MEM_LINES = 8;

window.onload = () => {
  const eventSource = new EventSource('events');
  window.onbeforeunload = () => eventSource.close();

  eventSource.addEventListener('started', handleEmulatorStarted);
  eventSource.addEventListener('paused', handleEmulatorPaused);
  eventSource.addEventListener('status', handleStatusUpdate);

  fillAddressLabels(getAddress(), MEM_LINES);
  const addressField = document.getElementById('address');
  addressField.addEventListener('input', () => {
    const address = getAddress();
    fillAddressLabels(address, MEM_LINES);
    refreshMemory(address);
  });
  addressField.addEventListener('wheel', memoryScrollWheel);
  document.getElementById('memoryHex').addEventListener('wheel', memoryScrollWheel);
  document.getElementById('memoryASCII').addEventListener('wheel', memoryScrollWheel);
  document.getElementById('addressLabels').addEventListener('wheel', memoryScrollWheel);
  addressField.addEventListener('wheel', memoryScrollWheel);
};

function memoryScrollWheel(event) {
  event.preventDefault();
  const v = parseInt(address.value, 16) + (event.deltaY < 0 ? -8 : 8) & 0xFFFF;

  address.value = v.toString(16).toUpperCase();
  fillAddressLabels(v, MEM_LINES);
  refreshMemory(v);
}

let status = "";

function handleEmulatorStarted(event) {
  const disableButton = button => button.setAttribute('disabled', true);

  if (status !== 'started') {
    status = 'started';
    document.getElementsByName('run')
      .forEach(element => element.innerText = 'Pause');
    document.getElementsByName('step').forEach(disableButton);
    document.getElementsByName('stepOver').forEach(disableButton);
    document.getElementsByName('stepOut').forEach(disableButton);
  }
}

function handleEmulatorPaused(event) {
  const enableButton = button => button.removeAttribute("disabled");

  refreshMemory(getAddress());

  if (status !== "paused") {
    status = "paused";
    document.getElementsByName('run')
      .forEach(element => element.innerText = "Run");
    document.getElementsByName('step').forEach(enableButton);
    document.getElementsByName('stepOver').forEach(enableButton);
    document.getElementsByName('stepOut').forEach(enableButton);
  }

}

let previousData = {};

function handleStatusUpdate(event) {
  refreshMemory(getAddress());
  const data = JSON.parse(event.data);
  for (let key of Object.keys(data)) {
    const value = data[key];
    const element = document.getElementById(key);
    if (!element) console.log("bad key " + key);
    if (previousData[key] !== value) {
      previousData[key] = value;
      element.classList.add('changed');
      element.firstChild.nodeValue = value;
    } else {
      element.classList.remove('changed');
    }
  }
}

function decodeASCII(c) {
  return c <= 0x20 || c > 0x7E ? '.' : String.fromCodePoint(c);
}

function getAddress() {
  const address = document.getElementById('address').value;
  return !address ? 0 : parseInt(address, 16);
}

function fillAddressLabels(baseAddress, lines) {
  const labels = [];
  for (let i = 0; i < lines; i++) {
    labels[i] = (baseAddress + (8 * i) & 0xFFFF).toString(16).toUpperCase().padStart(4, '0');
  }
  document.getElementById('addressLabels').innerText = labels.join('\n');
}

function refreshMemory(baseAddress) {
  const xhr = new XMLHttpRequest();
  xhr.onreadystatechange = () => {
    if (xhr.readyState == XMLHttpRequest.DONE && xhr.status == 200) {
      document.getElementById('memoryHex').innerText = xhr.responseText;
      document.getElementById('memoryASCII').innerText =
        xhr.responseText.split('\n')
          .map(line => line.split(' ')
            .map(c => decodeASCII(parseInt(c, 16)))
            .join(''))
          .join('\n');
    }
  };

  xhr.open("GET",
    "memory?address=" + baseAddress.toString(16) +
    "&lines=" + MEM_LINES, true);
  xhr.send();
}
