'use strict';

window.onload = () => {
  const eventSource = new EventSource('events');
  window.onbeforeunload = () => eventSource.close();

  const memory = new Memory();
  const disassembly = new Disassembly();
  initStatus(eventSource, memory, disassembly);

  eventSource.addEventListener('breakpoint-added', disassembly.breakPointAdded);
  eventSource.addEventListener('breakpoint-removed', disassembly.breakPointRemoved);
};

/*****************************************************************************
 * STATUS
 *****************************************************************************/
function initStatus(eventSource, memory, disassembly) {
  let currentStatus = "";
  let currentStatusData = {};

  eventSource.addEventListener('started', onRun);
  eventSource.addEventListener('paused', onPause);
  eventSource.addEventListener('status', onUpdate);

  function onRun() {
    const disableButton = button => button.setAttribute('disabled', true);

    if (currentStatus !== 'started') {
      currentStatus = 'started';
      document.getElementsByName('run')
        .forEach(element => {
          element.firstChild.src = "svg/pause";
          element.lastChild.nodeValue = "Pause";
        });
      document.getElementsByName('step').forEach(disableButton);
      document.getElementsByName('stepOver').forEach(disableButton);
      document.getElementsByName('stepOut').forEach(disableButton);
    }
  }

  function onPause(event) {
    const enableButton = button => button.removeAttribute("disabled");
    const data = JSON.parse(event.data);

    memory.refresh();
    disassembly.setPC(data);
    disassembly.revealPC();
    document.getElementById('disassemblyAddress').value = formatLongAddress(data);

    if (currentStatus !== "paused") {
      currentStatus = "paused";
      document.getElementsByName('run')
        .forEach(element => {
          element.firstChild.src = "svg/run";
          element.lastChild.nodeValue = "Run";
        });
      document.getElementsByName('step').forEach(enableButton);
      document.getElementsByName('stepOver').forEach(enableButton);
      document.getElementsByName('stepOut').forEach(enableButton);
    }
  }

  function onUpdate(event) {
    memory.refresh();
    const data = JSON.parse(event.data);
    for (let key of Object.keys(data)) {
      const value = data[key];
      const element = document.getElementById(key);
      if (!element) console.log("bad key " + key);
      if (currentStatusData[key] !== value) {
        currentStatusData[key] = value;
        element.classList.add('changed');
        element.firstChild.nodeValue = value;
      } else {
        element.classList.remove('changed');
      }
    }
  }
}

/*****************************************************************************
 * MEMORY
 *****************************************************************************/
function Memory() {
  const LINES = 10;
  const addressField = document.getElementById('address');

  this.refresh = () => refresh(getAddress());

  fillLabels(getAddress(), LINES);

  addressField.addEventListener('input', () => {
    const address = getAddress();
    fillLabels(address, LINES);
    refresh(address);
  });
  addressField.addEventListener('wheel', onWheel);
  addressField.addEventListener('keydown', event => {
    switch (event.code) {
      case 'ArrowUp': return scroll(-8);
      case 'ArrowDown': return scroll(8);
      case 'PageUp': return scroll(-8 * LINES);
      case 'PageDown': return scroll(8 * LINES);
    }
  });
  document.getElementById('memoryHex').addEventListener('wheel', onWheel);
  document.getElementById('memoryASCII').addEventListener('wheel', onWheel);
  document.getElementById('addressLabels').addEventListener('wheel', onWheel);

  function onWheel(event) {
    event.preventDefault();
    scroll(event.deltaY < 0 ? -8 : 8);
  }

  function scroll(amount) {
    const v = parseInt('0' + address.value, 16) + amount & 0xFFFF;
    address.value = v.toString(16).toUpperCase();
    fillLabels(v, LINES);
    refresh(v);
  }

  function decodeASCII(c) {
    return c <= 0x20 || c > 0x7E ? '.' : String.fromCodePoint(c);
  }

  function getAddress() {
    const address = addressField.value;
    return !address ? 0 : parseInt('0' + address, 16);
  }

  function fillLabels(baseAddress, lines) {
    const labels = [];
    for (let i = 0; i < lines; i++) {
      labels[i] = (baseAddress + (8 * i) & 0xFFFF).toString(16).toUpperCase().padStart(4, '0');
    }
    document.getElementById('addressLabels').innerText = labels.join('\n');
  }

  function refresh(baseAddress) {
    httpGET("memory?address=" + baseAddress.toString(16) + "&lines=" + LINES,
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
}

/*****************************************************************************
 * Disassembly
 *****************************************************************************/
function Disassembly() {
  const DLINES = 20;

  const state = {
    lines: [], // { field :: { address, etc. }, li :: Element<li> }
    pc: { bank: 0, offset: 0 },
  };

  this.setPC = setPC;
  this.revealPC = () => {
    if (!getVisibleLineAt(state.pc)) jumpTo(state.pc);
  };
  this.breakPointAdded = function (event) {
    const line = getVisibleLineAt(JSON.parse(event.data));
    if (line) {
      line.li.querySelectorAll('input.breakpoint').forEach(button => button.classList.add('set'));
    }
  };

  this.breakPointRemoved = function (event) {
    const line = getVisibleLineAt(JSON.parse(event.data));
    if (line) {
      line.li.querySelectorAll('input.breakpoint').forEach(button => button.classList.remove('set'));
    }
  };

  const gotoPCButton = document.getElementsByName('toPC');
  gotoPCButton.forEach(element => element.onclick = () => jumpTo(state.pc));

  document.getElementById('disassemblyList').addEventListener('wheel', onWheel);

  const disassemblyAddress = document.getElementById('disassemblyAddress');
  disassemblyAddress.addEventListener('input', event => {
    jumpTo(parseLongAddress(
      state.lines[0].field.address,
      disassemblyAddress.value));
  });
  disassemblyAddress.addEventListener('wheel', onWheel);
  disassemblyAddress.addEventListener('keydown', event => {
    switch (event.code) {
      case 'ArrowUp': return scroll(-1);
      case 'ArrowDown': return scroll(1);
      case 'PageUp': return scroll(-DLINES);
      case 'PageDown': return scroll(DLINES);
    }
  });

  function getVisibleLineAt(address) {
    for (const line of state.lines) {
      if (sameAddress(line.field.address, address)) return line;
    }
    return null;
  }

  function toggleBreakpointAt(address) {
    const uri = '/breakpoints?bank=' + address.bank.toString(16) +
      '&offset=' + address.offset.toString(16);
    if (this.classList.contains('set')) {
      httpPOST(uri, 'unset');
    } else {
      httpPOST(uri, 'set');
    }
  }

  function onWheel(event) {
    event.preventDefault();
    switch (event.deltaMode) {
      case 0: // DOM_DELTA_PIXEL
        return scroll(Math.round(event.deltaY / 16));
      case 1: // DOM_DELTA_LINE
        return scroll(Math.round(event.deltaY));
      case 2: // DOM_DELTA_PAGE
        return scroll(Math.round(event.deltaY * DLINES));
    }
  }

  function formatDisassemblyField(field, breakpoints) {
    const li = document.createElement('li');

    const breakpoint = document.createElement('input');
    breakpoint.type = 'button';
    breakpoint.title = 'Toggle break point';
    breakpoint.classList.add('disassemblyButton', 'breakpoint');
    breakpoint.onclick = () => toggleBreakpointAt.call(breakpoint, field.address);
    if (containsAddress(field.address, breakpoints)) {
      breakpoint.classList.add('set');
    }

    const runTo = document.createElement('input');
    runTo.type = 'button';
    runTo.title = 'Run to here';
    runTo.classList.add('disassemblyButton', 'runTo');
    runTo.onclick = () => {
      const address = field.address;
      httpPOST('/', 'runTo=&bank=' +
        address.bank.toString(16) + '&offset=' + address.offset.toString(16));
    };

    li.setAttribute('data-address', formatLongAddress(field.address));
    li.appendChild(breakpoint);
    li.appendChild(runTo);

    const instruction = document.createElement('span');
    if (field.text === 'db') {
      instruction.classList.add('data');
      instruction.innerText = field.text + " " + field.bytes;
      li.appendChild(instruction);

    } else {
      instruction.classList.add('instruction');
      const parameters = field.p.map(x => x.text).join(", ");
      instruction.innerText = field.text + (parameters ? " " + parameters : "") + "\t";

      const bytes = document.createElement('span');
      bytes.classList.add('bytes');
      bytes.innerText = '; ' + field.bytes

      li.appendChild(instruction);
      li.appendChild(bytes);
    }

    if (field.overlap) {
      instruction.innerText = '; ' + instruction.innerText;
      instruction.classList.add('overlapping');

      const overlapping = document.createElement('span');
      overlapping.classList.add('overlapping');
      overlapping.innerText = "\t(overlapping)";
      li.appendChild(overlapping);
    }

    return li;
  }

  function setPC(pc) {
    const oldPCLine = getVisibleLineAt(state.pc);
    if (oldPCLine) oldPCLine.li.classList.remove('pc');

    state.pc = pc;
    const pcLine = getVisibleLineAt(pc);
    if (pcLine) pcLine.li.classList.add('pc');
  }

  const scrollQueue = [];
  function scroll(amount) {
    scrollQueue.push(amount);
    if (scrollQueue.length === 1) {
      doScroll(amount);
    }

    function doScroll(amount) {
      const baseAddress = (amount < 0
        ? state.lines[0]
        : state.lines[state.lines.length - 1]).field.address;

      httpGET(
        "disassembly?bank=" + baseAddress.bank.toString(16) +
        "&offset=" + baseAddress.offset.toString(16) +
        "&n=" + amount,
        text => {
          const disassembly = JSON.parse(text);
          const newFields = disassembly.fields.slice(1)
            .map(field => {
              return { field: field, li: formatDisassemblyField(field, disassembly.breakpoints) }
            });

          const ul = document.getElementById('disassemblyList');
          if (amount < 0) {
            newFields.reverse();
            state.lines = newFields.concat(state.lines).slice(0, DLINES);
            ul.prepend(...newFields.map(line => line.li));
            while (ul.childNodes.length > DLINES) {
              ul.removeChild(ul.lastChild);
            }
          } else {
            state.lines = state.lines.concat(newFields).slice(newFields.length);
            ul.append(...newFields.map(line => line.li));
            while (ul.childNodes.length > DLINES) {
              ul.removeChild(ul.firstChild);
            }
          }

          document.getElementById('disassemblyAddress').value =
            formatLongAddress(state.lines[0].field.address);

          setPC(state.pc);

          scrollQueue.shift();
          if (scrollQueue.length > 0) {
            doScroll(scrollQueue[0]);
          }
        },
        error => {
          scrollQueue.slice(0, 0);
        }
      );
    }
  }

  function jumpTo(address) {
    httpGET(
      "disassembly?bank=" + address.bank.toString(16) +
      "&offset=" + address.offset.toString(16) +
      "&n=" + DLINES,
      text => {
        const disassembly = JSON.parse(text);

        const ul = document.getElementById('disassemblyList');
        ul.innerHTML = '';
        state.lines = [];
        for (const field of disassembly.fields.slice(0, DLINES)) {
          const li = formatDisassemblyField(field, disassembly.breakpoints);
          ul.appendChild(li);
          state.lines.push({ field: field, li: li });
        }

        setPC(state.pc);
      });
  }
}


/*****************************************************************************
 * Utility
 *****************************************************************************/
function sameAddress(a, b) {
  return a.offset === b.offset && a.bank === b.bank;
}

function containsAddress(a, array) {
  for (const b of array) {
    if (sameAddress(a, b)) return true;
  }
  return false;
}

function formatLongAddress(a) {
  return (a.bank === 0xFFFF ? "BOOT" : a.bank.toString(16).padStart(4, '0').toUpperCase()) +
    ":" + a.offset.toString(16).padStart(4, '0').toUpperCase();
}

function parseLongAddress(defaultAddress, address) {
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

function httpGET(uri, responseHandler, errorHandler) {
  const xhr = new XMLHttpRequest();
  if (errorHandler) xhr.onerror = errorHandler;
  xhr.onreadystatechange = () => {
    if (xhr.readyState == XMLHttpRequest.DONE && xhr.status == 200) {
      responseHandler(xhr.responseText);
    }
  };

  xhr.open("GET", uri, true);
  xhr.send();
}

function httpPOST(uri, body) {
  const xhr = new XMLHttpRequest();
  xhr.open("POST", uri, true);
  if (body) {
    xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
    xhr.send(body);
  } else {
    xhr.send();
  }
}
