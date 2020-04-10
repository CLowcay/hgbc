const MEM_LINES = 10;

window.onload = () => {
  const eventSource = new EventSource('events');
  window.onbeforeunload = () => eventSource.close();

  eventSource.addEventListener('started', handleEmulatorStarted);
  eventSource.addEventListener('paused', handleEmulatorPaused);
  eventSource.addEventListener('status', handleStatusUpdate);

  initMemoryPanel();
  initDisassemblyPanel();
};

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
  const data = JSON.parse(event.data);

  refreshMemory(getAddress());
  refreshDisassembly(data);
  document.getElementById('disassemblyAddress').value = formatAddress(data);

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

function initMemoryPanel() {
  fillAddressLabels(getAddress(), MEM_LINES);
  const addressField = document.getElementById('address');
  addressField.addEventListener('input', () => {
    const address = getAddress();
    fillAddressLabels(address, MEM_LINES);
    refreshMemory(address);
  });
  addressField.addEventListener('wheel', memoryScrollWheel);
  addressField.addEventListener('keydown', event => {
    switch (event.code) {
      case 'ArrowUp': return scrollMemory(-8);
      case 'ArrowDown': return scrollMemory(8);
      case 'PageUp': return scrollMemory(-8 * MEM_LINES);
      case 'PageDown': return scrollMemory(8 * MEM_LINES);
    }
  });
  document.getElementById('memoryHex').addEventListener('wheel', memoryScrollWheel);
  document.getElementById('memoryASCII').addEventListener('wheel', memoryScrollWheel);
  document.getElementById('addressLabels').addEventListener('wheel', memoryScrollWheel);
}

function memoryScrollWheel(event) {
  event.preventDefault();
  scrollMemory(event.deltaY < 0 ? -8 : 8);
}

function scrollMemory(amount) {
  const v = parseInt('0' + address.value, 16) + amount & 0xFFFF;
  address.value = v.toString(16).toUpperCase();
  fillAddressLabels(v, MEM_LINES);
  refreshMemory(v);
}

function decodeASCII(c) {
  return c <= 0x20 || c > 0x7E ? '.' : String.fromCodePoint(c);
}

function getAddress() {
  const address = document.getElementById('address').value;
  return !address ? 0 : parseInt('0' + address, 16);
}

function fillAddressLabels(baseAddress, lines) {
  const labels = [];
  for (let i = 0; i < lines; i++) {
    labels[i] = (baseAddress + (8 * i) & 0xFFFF).toString(16).toUpperCase().padStart(4, '0');
  }
  document.getElementById('addressLabels').innerText = labels.join('\n');
}

function refreshMemory(baseAddress) {
  ajax("GET",
    "memory?address=" + baseAddress.toString(16) +
    "&lines=" + MEM_LINES,
    text => {
      document.getElementById('memoryHex').innerText = text;
      document.getElementById('memoryASCII').innerText =
        text.split('\n')
          .map(line => line.split(' ')
            .map(c => decodeASCII(parseInt(c, 16)))
            .join(''))
          .join('\n');
    });
}

// TODO: remove this
const DLINES = 20;

const disassemblyState = {
  lines: [],
  pc: { bank: 0, offset: 0 },
};

function parseAddress(defaultAddress, address) {
  const parts = address.split(':');
  if (parts.length === 1) {
    return { bank: defaultAddress.bank, offset: parseInt('0' + parts[0], 16) };
  } else if (parts.length === 2) {
    if (parts[0].toUpperCase() === 'BOOT') {
      return { bank: 0xFFFF, offset: parseInt('0' + parts[1], 16) };
    } else {
      return { bank: parseInt('0' + parts[0], 16), offset: parseInt('0' + parts[1], 16) };
    }
  } else {
    return defaultAddress;
  }
}

function initDisassemblyPanel() {
  const disassemblyAddress = document.getElementById('disassemblyAddress');
  disassemblyAddress.addEventListener('input', event => {
    setDisassemblyTop(parseAddress(
      disassemblyState.lines[0].field.address,
      disassemblyAddress.value));
  });

  disassemblyAddress.addEventListener('wheel', disassemblerScrollWheel);
  disassemblyAddress.addEventListener('keydown', event => {
    switch (event.code) {
      case 'ArrowUp': return scrollDisassembly(-1);
      case 'ArrowDown': return scrollDisassembly(1);
      case 'PageUp': return scrollDisassembly(-DLINES);
      case 'PageDown': return scrollDisassembly(DLINES);
    }
  });
  document.getElementById('disassemblyList')
    .addEventListener('wheel', disassemblerScrollWheel);
}

function disassemblerScrollWheel(event) {
  event.preventDefault();
  switch (event.deltaMode) {
    case 0: // DOM_DELTA_PIXEL
      return scrollDisassembly(Math.round(event.deltaY / 16));
    case 1: // DOM_DELTA_LINE
      return scrollDisassembly(Math.round(event.deltaY));
    case 2: // DOM_DELTA_PAGE
      return scrollDisassembly(Math.round(event.deltaY * DLINES));
  }
}

function sameAddress(a, b) {
  return a.offset === b.offset && a.bank === b.bank;
}

function formatAddress(a) {
  return (a.bank === 0xFFFF ? "BOOT" : a.bank.toString(16).padStart(4, '0').toUpperCase()) +
    ":" + a.offset.toString(16).padStart(4, '0').toUpperCase();
}

function formatField(field) {
  const li = document.createElement('li');

  const instruction = document.createElement('span');
  instruction.setAttribute('class', 'instruction');
  instruction.innerText = field.text + "\t";

  const bytes = document.createElement('span');
  bytes.setAttribute('class', 'bytes');
  bytes.innerText = "; " + field.bytes

  li.setAttribute('data-address', formatAddress(field.address));
  li.appendChild(instruction);
  li.appendChild(bytes);
  return li;
}

function getVisibleLineAt(address) {
  for (const line of disassemblyState.lines) {
    if (sameAddress(line.field.address, address)) return line;
  }
  return null;
}

function setDisassemblyPC(pc) {
  const oldPCLine = getVisibleLineAt(disassemblyState.pc);
  if (oldPCLine) {
    oldPCLine.li.classList.remove('pc');
  }

  const pcLine = getVisibleLineAt(pc);
  if (pcLine) {
    pcLine.li.classList.add('pc');
    disassemblyState.pc = pc;
  }
}

const scrollQueue = [];

function scrollDisassembly(amount) {
  scrollQueue.push(amount);
  if (scrollQueue.length === 1) {
    actuallyScrollDisassembly(amount);
  }

  function actuallyScrollDisassembly(amount) {
    const baseAddress = (amount < 0
      ? disassemblyState.lines[0]
      : disassemblyState.lines[disassemblyState.lines.length - 1]).field.address;

    ajax("GET",
      "disassembly?bank=" + baseAddress.bank.toString(16) +
      "&offset=" + baseAddress.offset.toString(16) +
      "&n=" + amount,
      text => {
        const disassembly = JSON.parse(text).slice(1)
          .map(field => { return { field: field, li: formatField(field) } });

        const ul = document.getElementById('disassemblyList');
        if (amount < 0) {
          disassembly.reverse();
          disassemblyState.lines = disassembly.concat(disassemblyState.lines).slice(0, DLINES);
          ul.prepend(...disassembly.map(line => line.li));
          while (ul.childNodes.length > DLINES) {
            ul.removeChild(ul.lastChild);
          }
        } else {
          disassemblyState.lines = disassemblyState.lines.concat(disassembly).slice(disassembly.length);
          ul.append(...disassembly.map(line => line.li));
          while (ul.childNodes.length > DLINES) {
            ul.removeChild(ul.firstChild);
          }
        }

        document.getElementById('disassemblyAddress').value =
          formatAddress(disassemblyState.lines[0].field.address);

        setDisassemblyPC(disassemblyState.pc);

        scrollQueue.shift();
        if (scrollQueue.length > 0) {
          actuallyScrollDisassembly(scrollQueue[0]);
        }
      },
      error => {
        scrollQueue.slice(0, 0);
      }
    );
  }
}

function setDisassemblyTop(address) {
  ajax("GET",
    "disassembly?bank=" + address.bank.toString(16) +
    "&offset=" + address.offset.toString(16) +
    "&n=" + DLINES,
    text => {
      const disassembly = JSON.parse(text).slice(0, DLINES);

      const ul = document.getElementById('disassemblyList');
      ul.innerHTML = '';
      disassemblyState.lines = [];
      for (const field of disassembly) {
        const li = formatField(field);
        ul.appendChild(li);
        disassemblyState.lines.push({ field: field, li: li });
      }

      setDisassemblyPC(disassemblyState.pc);
    });
}

function refreshDisassembly(pc) {
  const newPCLine = getVisibleLineAt(pc);
  if (newPCLine) {
    setDisassemblyPC(pc);
  } else {
    ajax("GET",
      "disassembly?bank=" + pc.bank.toString(16) +
      "&offset=" + pc.offset.toString(16) +
      "&n=" + DLINES,
      text => {
        const disassembly = JSON.parse(text).slice(0, DLINES);

        const ul = document.getElementById('disassemblyList');
        ul.innerHTML = '';
        disassemblyState.lines = [];
        for (const field of disassembly) {
          const li = formatField(field);
          ul.appendChild(li);
          disassemblyState.lines.push({ field: field, li: li });
        }

        setDisassemblyPC(pc);
      });
  }
}

function ajax(method, uri, responseHandler, errorHandler) {
  const xhr = new XMLHttpRequest();
  if (errorHandler) xhr.onerror = errorHandler;
  xhr.onreadystatechange = () => {
    if (xhr.readyState == XMLHttpRequest.DONE && xhr.status == 200) {
      responseHandler(xhr.responseText);
    }
  };

  xhr.open(method, uri, true);
  xhr.send();
}
