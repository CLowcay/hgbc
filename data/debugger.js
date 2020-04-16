'use strict';

window.onload = () => {
  const eventSource = new EventSource('events');
  window.onbeforeunload = () => eventSource.close();

  const memory = new Memory();
  const disassembly = new Disassembly();
  initStatus(eventSource, memory, disassembly);

  eventSource.addEventListener('breakpoint-added',
    event => disassembly.breakPointAdded(JSON.parse(event.data)));
  eventSource.addEventListener('breakpoint-removed',
    event => disassembly.breakPointRemoved(JSON.parse(event.data)));

  document.getElementById('run').onclick = () => httpPOST('/', 'run');
  document.getElementById('step').onclick = () => httpPOST('/', 'step');
  document.getElementById('stepOver').onclick = () => httpPOST('/', 'stepOver');
  document.getElementById('stepOut').onclick = () => httpPOST('/', 'stepOut');
  document.getElementById('restart').onclick = () => httpPOST('/', 'restart');
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

  const runButton = document.getElementById('run');
  const stepButton = document.getElementById('step');
  const stepOverButton = document.getElementById('stepOver');
  const stepOutButton = document.getElementById('stepOut');

  function onRun() {
    if (currentStatus !== 'started') {
      currentStatus = 'started';
      runButton.firstChild.src = "svg/pause";
      runButton.lastChild.nodeValue = "Pause";
      stepButton.setAttribute('disabled', true);
      stepOverButton.setAttribute('disabled', true);
      stepOutButton.setAttribute('disabled', true);
    }
  }

  function onPause(event) {
    const data = JSON.parse(event.data);

    memory.refresh();
    disassembly.setPC(data);
    disassembly.revealPC();

    if (currentStatus !== "paused") {
      currentStatus = "paused";
      runButton.firstChild.src = "svg/run";
      runButton.lastChild.nodeValue = "Run";
      stepButton.removeAttribute('disabled');
      stepOverButton.removeAttribute('disabled');
      stepOutButton.removeAttribute('disabled');
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
    const v = (getAddress() + amount) & 0xFFFF;
    addressField.value = formatShortAddress(v);
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
      labels[i] = formatShortAddress(baseAddress + (8 * i) & 0xFFFF);
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
  const LINES = 20;

  const state = {
    lines: [], // { address, field :: { text, data } | label :: string, li :: Element<li> }
    pc: { bank: 0, offset: 0 },
    selection: { bank: 0, offset: 0 }
  };

  this.setPC = setPC;
  this.revealPC = () => {
    if (!getVisibleLineAt(state.pc)) jumpTo(state.pc);
    setSelection(state.pc);
  };
  this.breakPointAdded = function (address) {
    const line = getVisibleLineAt(address);
    if (line) line.li.querySelector('input.breakpoint').classList.add('set');
  };
  this.breakPointRemoved = function (address) {
    const line = getVisibleLineAt(address);
    if (line) line.li.querySelector('input.breakpoint').classList.remove('set');
  };
  this.labelUpdated = function (label) {  // {address, text}

  }
  this.labelRemoved = function (label) {

  }

  // Set up the disassembler buttons.
  document.getElementById('toPC').onclick = () => {
    if (!getVisibleLineAt(state.pc)) jumpTo(state.pc);
    setSelection(state.pc);
  };
  document.getElementById('runTo').onclick = () => runToAddress(state.selection);
  document.getElementById('breakpoint').onclick = () => {
    if (!getVisibleLineAt(state.selection)) jumpTo(state.selection);
    toggleBreakpointAtVisibleAddress(state.selection);
  };
  // TODO: implement labels
  //document.getElementById('label')

  document.getElementById('disassemblyList').addEventListener('wheel', onWheel);

  const disassemblyAddress = document.getElementById('disassemblyAddress');
  disassemblyAddress.addEventListener('input', event => {
    const address = parseLongAddress(state.lines[0].address, disassemblyAddress.value);
    jumpTo(address);
    setSelection(address);
  });
  disassemblyAddress.addEventListener('keydown', event => {
    switch (event.code) {
      case 'ArrowUp':
        event.preventDefault();
        return moveSelection(-1);
      case 'ArrowDown':
        event.preventDefault();
        return moveSelection(1);
      case 'Enter':
        event.preventDefault();
        return moveSelection(0);
    }
  });

  const disassemblyWindow = document.querySelector('div.disassembly div.window');
  disassemblyWindow.addEventListener('keydown', event => {
    switch (event.code) {
      case 'ArrowUp':
        event.preventDefault();
        return moveSelection(-1);
      case 'ArrowDown':
        event.preventDefault();
        return moveSelection(1);
      case 'PageUp':
        event.preventDefault();
        return moveSelection(-LINES);
      case 'PageDown':
        event.preventDefault();
        return moveSelection(LINES);
      case 'Home':
      case 'End':
      case 'ArrowLeft':
      case 'ArrowRight':
      case 'Space':
      case 'Enter':
        event.preventDefault();
        return moveSelection(0);
    }
  });

  function getVisibleLineAt(address) {
    return state.lines.find(line => sameAddress(line.address, address));
  }

  function toggleBreakpointAtVisibleAddress(address) {
    const button = getVisibleLineAt(state.selection).li.querySelector('input.breakpoint');
    const uri = '/breakpoints?bank=' + address.bank.toString(16) +
      '&offset=' + address.offset.toString(16);
    if (button.classList.contains('set')) {
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
        return scroll(Math.round(event.deltaY * LINES));
    }
  }

  function formatDisassemblyField(field, breakpoints) {
    const li = document.createElement('li');

    const breakpoint = document.createElement('input');
    breakpoint.type = 'button';
    breakpoint.title = 'Toggle break point';
    breakpoint.classList.add('disassemblyButton', 'breakpoint');
    breakpoint.onclick = () => toggleBreakpointAtVisibleAddress(field.address);
    if (containsAddress(field.address, breakpoints)) {
      breakpoint.classList.add('set');
    }

    const runTo = document.createElement('input');
    runTo.type = 'button';
    runTo.title = 'Run to here';
    runTo.classList.add('disassemblyButton', 'runTo');
    runTo.onclick = () => runToAddress(field.address);

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

    li.onmousedown = () => setSelection(field.address);
    li.onmouseup = () => setSelection(field.address);
    li.onmousemove = event => {
      if (event.buttons > 0) setSelection(field.address);
    };

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
  function scroll(amount, continuation) {
    scrollQueue.push(amount);
    if (scrollQueue.length === 1) {
      doScroll(amount);
    }

    function doScroll(amount) {
      const baseAddress = (amount < 0
        ? state.lines[0]
        : state.lines[state.lines.length - 1]).address;

      httpGET(
        "disassembly?bank=" + baseAddress.bank.toString(16) +
        "&offset=" + baseAddress.offset.toString(16) +
        "&n=" + amount,
        text => {
          const disassembly = JSON.parse(text);
          const newFields = disassembly.fields.slice(1)
            .map(field => {
              return {
                address: field.address,
                field: field,
                li: formatDisassemblyField(field, disassembly.breakpoints)
              }
            });

          const ul = document.getElementById('disassemblyList');
          if (amount < 0) {
            newFields.reverse();
            state.lines = newFields.concat(state.lines).slice(0, LINES);
            ul.prepend(...newFields.map(line => line.li));
            while (ul.childNodes.length > LINES) {
              ul.removeChild(ul.lastChild);
            }
          } else {
            state.lines = state.lines.concat(newFields).slice(newFields.length);
            ul.append(...newFields.map(line => line.li));
            while (ul.childNodes.length > LINES) {
              ul.removeChild(ul.firstChild);
            }
          }

          setPC(state.pc);
          setSelection(state.selection);
          if (continuation) continuation();

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

  function runToAddress(address) {
    httpPOST('/', 'runTo=&bank=' +
      address.bank.toString(16) + '&offset=' + address.offset.toString(16));
  }

  function jumpTo(address, continuation) {
    httpGET(
      "disassembly?bank=" + address.bank.toString(16) +
      "&offset=" + address.offset.toString(16) +
      "&n=" + LINES,
      text => {
        const disassembly = JSON.parse(text);

        const ul = document.getElementById('disassemblyList');
        ul.innerHTML = '';
        state.lines = [];
        for (const field of disassembly.fields.slice(0, LINES)) {
          const li = formatDisassemblyField(field, disassembly.breakpoints);
          ul.appendChild(li);
          state.lines.push({ address: field.address, field: field, li: li });
        }

        setPC(state.pc);
        setSelection(state.selection);
        if (continuation) continuation();
      });
  }

  function setSelection(address) {
    const oldSelection = getVisibleLineAt(state.selection);
    if (oldSelection) oldSelection.li.classList.remove('selected');

    const selection = getVisibleLineAt(address);
    if (selection) selection.li.classList.add('selected');

    if (!sameAddress(state.selection, address)) {
      state.selection = address;
      disassemblyAddress.value = formatLongAddress(address);
    }
  }

  function moveSelection(amount) {
    const i = state.lines.findIndex(line => sameAddress(line.address, state.selection));
    if (i === -1) {
      jumpTo(state.selection, () => moveSelection(amount));
    } else {
      const iNext = i + amount;
      if (iNext < 0) {
        scroll(iNext, () => setSelection(state.lines[0].address));
      } else if (iNext >= LINES) {
        scroll(iNext - LINES + 1, () => setSelection(state.lines[LINES - 1].address));
      } else {
        setSelection(state.lines[iNext].address);
      }
    }
  }
}

/*****************************************************************************
 * Utility
 *****************************************************************************/
function sameAddress(a, b) {
  return a.offset === b.offset && a.bank === b.bank;
}

function containsAddress(a, array) {
  return array.some(b => sameAddress(a, b));
}

function formatLongAddress(a) {
  return (a.bank === 0xFFFF ? "BOOT" : formatShortAddress(a.bank)) +
    ":" + formatShortAddress(a.offset);
}

function formatShortAddress(a) {
  return a.toString(16).toUpperCase().padStart(4, '0');
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
