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
  eventSource.addEventListener('label-added',
    event => disassembly.labelUpdated(JSON.parse(event.data)));
  eventSource.addEventListener('label-removed',
    event => disassembly.labelRemoved(JSON.parse(event.data)));

  document.getElementById('run').onclick = () => httpPOST('/', 'run');
  document.getElementById('step').onclick = () => httpPOST('/', 'step');
  document.getElementById('stepOver').onclick = () => httpPOST('/', 'stepOver');
  document.getElementById('stepOut').onclick = () => httpPOST('/', 'stepOut');
  document.getElementById('restart').onclick = () => httpPOST('/', 'restart');

  document.addEventListener('keydown', event => {
    switch (event.code) {
      case 'KeyG':
        if (event.ctrlKey) {
          event.preventDefault();
          document.getElementById('disassemblyAddress').focus();
        }
    }
  });
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
  const LINE_HEIGHT = 1.5;

  const state = {
    lines: [], // { address, field :: { text, data } | label :: string, li :: Element<li> }
    scrollIndex: 0,
    maxScrollIndex: LINES,
    pc: { bank: 0, offset: 0 },
    selection: { isLabel: false, address: { bank: 0, offset: 0 } },
    labels: {} // address => text
  };

  let newestLabel = undefined;

  let uninitialized = true;
  this.setPC = setPC;
  this.revealPC = () => {
    jumpTo(state.pc, () => {
      if (uninitialized) {
        setSelection({ address: state.pc, isLabel: false });
        uninitialized = false;
      }
    });
  };

  this.breakPointAdded = function (address) {
    const li = getFieldLI(address);
    if (li) li.querySelector('input.breakpoint').classList.add('set');
  };
  this.breakPointRemoved = function (address) {
    const li = getFieldLI(address);
    if (li) li.querySelector('input.breakpoint').classList.remove('set');
  };
  this.labelUpdated = function (label) {
    state.labels[formatLongAddress(label.address)] = label.text;
    refreshListing();
  };
  this.labelRemoved = function (address) {
    delete state.labels[formatLongAddress(address)];
    refreshListing();
  };

  // Set up the disassembler buttons.
  document.getElementById('toPC').onclick = () => {
    jumpTo(state.pc, () => setSelection({ address: state.pc, isLabel: false }));
  }
  document.getElementById('runTo').onclick = () => runToAddress(state.selection.address);
  document.getElementById('breakpoint').onclick = () =>
    jumpTo(state.selection.address, () =>
      toggleBreakpointAtVisibleAddress(state.selection.address));
  document.getElementById('label').onclick = () => {
    const address = state.selection.address;
    jumpTo(address);
    if (state.labels[formatLongAddress(address)] === undefined) {
      newestLabel = address;
      postLabelUpdate(address, "newLabel");
    } else {
      const position = { address: address, isLabel: true };
      const text = getLI(position).firstChild;
      setSelection(position);
      window.getSelection().setBaseAndExtent(text, 0, text, 1);
    }
  }

  const disassemblyWindow = document.querySelector('div.disassembly div.window');
  disassemblyWindow.addEventListener('wheel', onWheel);
  disassemblyWindow.addEventListener('keydown', event => {
    switch (event.key) {
      case 'ArrowUp':
        event.preventDefault();
        disassemblyWindow.focus();
        return moveSelection(-1);
      case 'ArrowDown':
        event.preventDefault();
        disassemblyWindow.focus();
        return moveSelection(1);
      case 'PageUp':
        event.preventDefault();
        disassemblyWindow.focus();
        return moveSelection(-LINES);
      case 'PageDown':
        event.preventDefault();
        return moveSelection(LINES);
      case '+':
        const address = state.selection.address;
        if (state.labels[formatLongAddress(address)] === undefined) {
          jumpTo(address);
          newestLabel = address;
          postLabelUpdate(address, "newLabel");
        }
        break;
      case 'Home':
      case 'End':
      case 'ArrowLeft':
      case 'ArrowRight':
      case ' ':
        if (!event.target.classList.contains('label')) {
          event.preventDefault();
          moveSelection(0);
          updateFocus();
        }
        break;
      case 'Enter':
        event.preventDefault();
        disassemblyWindow.focus();
        moveSelection(1);
    }
  });

  const disassemblyAddress = document.getElementById('disassemblyAddress');
  disassemblyAddress.addEventListener('blur', () => refreshDisassemblyAddress());
  disassemblyAddress.addEventListener('input', () => {
    const address = parseLongAddress(state.lines[0].address, disassemblyAddress.value);
    jumpTo(address, () => setSelection({ address: address, isLabel: false }));
  });
  disassemblyAddress.addEventListener('keydown', event => {
    switch (event.code) {
      case 'ArrowUp':
        event.preventDefault();
        disassemblyWindow.focus();
        moveSelection(-1);
        break;
      case 'ArrowDown':
        event.preventDefault();
        disassemblyWindow.focus();
        moveSelection(1);
        break;
      case 'Enter':
        event.preventDefault();
        disassemblyWindow.focus();
        moveSelection(0);
        break;
    }
  });

  // initialize the labels
  httpGET('labels', text => {
    JSON.parse(text).forEach(label =>
      state.labels[formatLongAddress(label.address)] = label.text);
    refreshListing();
  });

  function postLabelUpdate(address, text) {
    const uri = "label?bank=" + address.bank.toString(16) +
      "&offset=" + address.offset.toString(16);
    httpPOST(uri, "update=" + encodeURIComponent(text));
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

  function lineAt(position) {
    return state.lines.find(line =>
      sameAddress(line.address, position.address) &&
      position.isLabel === line.hasOwnProperty('label'));
  }

  function positionToIndex(position) {
    return state.lines.findIndex(line =>
      sameAddress(line.address, position.address) &&
      (line.hasOwnProperty('label') ? position.isLabel : true));
  }

  function indexToPosition(index) {
    const line = state.lines[index];
    return {
      address: line.address,
      isLabel: line.hasOwnProperty('label')
    };
  }

  function getLI(position) {
    const line = lineAt(position);
    return line ? line.li : undefined;
  }

  function getFieldLI(address) {
    return getLI({ address: address, isLabel: false });
  }

  function isVisible(position) {
    const i = positionToIndex(position);
    return i >= state.scrollIndex && i < state.scrollIndex + LINES;
  }

  function updateMaxScrollIndex() {
    state.maxScrollIndex = state.lines.length - LINES;
  }

  function toggleBreakpointAtVisibleAddress(address) {
    const button = getFieldLI(state.selection.address).querySelector('input.breakpoint');
    const uri = '/breakpoints?bank=' + address.bank.toString(16) +
      '&offset=' + address.offset.toString(16);
    if (button.classList.contains('set')) {
      httpPOST(uri, 'unset');
    } else {
      httpPOST(uri, 'set');
    }
  }

  function updateLabels(lines) {
    let i = 0;
    while (i < lines.length) {
      const line = lines[i];
      const label = state.labels[formatLongAddress(line.address)];
      if (label === undefined) {
        if (line.label === undefined) {
          i += 1;
        } else {
          lines.splice(i, 1);
        }
      } else {
        if (line.label === undefined) {
          lines.splice(i, 0, {
            address: line.address,
            label: label,
            li: createLabel(line.address, label)
          });
        } else if (line.li.firstChild.innerText !== label) {
          line.label = label;
          line.li.firstChild.innerText = label;
        }
        i += 2;
      }
    }
  }

  function refreshListing() {
    updateLabels(state.lines);
    updateMaxScrollIndex();
    const ul = document.getElementById('disassemblyList');
    ul.innerHTML = '';
    state.lines.forEach(line => ul.appendChild(line.li));
    if (newestLabel) {
      const position = { address: newestLabel, isLabel: true };
      const text = getLI(position).firstChild;
      setSelection(position);
      window.getSelection().setBaseAndExtent(text, 0, text, 1);
      newestLabel = undefined;
    }
  }

  function setScrollIndex(index) {
    state.scrollIndex = index;
    document.getElementById('disassemblyList').style.top =
      (LINE_HEIGHT * -index).toString() + 'em';
  }

  function setPC(pc) {
    const oldPCLine = getFieldLI(state.pc);
    if (oldPCLine) oldPCLine.classList.remove('pc');

    state.pc = pc;
    const pcLine = getFieldLI(pc);
    if (pcLine) pcLine.classList.add('pc');
  }

  function refreshDisassemblyAddress() {
    disassemblyAddress.value = formatLongAddress(state.selection.address);
  }

  function updateFocus() {
    if (state.selection.isLabel) {
      const li = getLI(state.selection);
      if (li && window.getSelection().isCollapsed) {
        const span = li.firstChild;
        span.contentEditable = true;
        span.focus();
      } else {
        disassemblyWindow.focus();
      }
    } else {
      disassemblyWindow.focus();
    }
  }

  function setSelection(position) {
    const oldLine = getLI(state.selection);
    if (oldLine) oldLine.classList.remove('selected');

    const newLine = getLI(position);
    if (newLine) newLine.classList.add('selected');

    state.selection = position;
    if (oldLine !== newLine && document.activeElement !== disassemblyAddress) {
      refreshDisassemblyAddress();
      updateFocus();
    }
  }

  function createLabel(address, labelText) {
    const li = document.createElement('li');
    const text = document.createElement('span');
    text.spellcheck = false;
    text.innerText = labelText;
    text.classList.add('label');
    text.addEventListener('keyup', event => event.stopPropagation());
    text.addEventListener('keydown', event => {
      switch (event.code) {
        case 'End':
          event.preventDefault();
          if (event.shiftKey) {
            window.getSelection().extend(text, 1);
          } else {
            window.getSelection().collapse(text, 1);
          }
          break;
      }
    });
    text.addEventListener('focus', () => setSelection({ address: address, isLabel: true }));
    text.addEventListener('blur', () => postLabelUpdate(address, text.innerText));

    const colon = document.createElement('span');
    colon.innerText = ':';
    li.appendChild(text);
    li.appendChild(colon);

    li.onmousedown = () => setSelection({ address: address, isLabel: true });
    li.onmouseup = () => setSelection({ address: address, isLabel: true });
    return li;
  }

  function createField(field, breakpoints) {
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

    li.onmousedown = () => setSelection({ address: field.address, isLabel: false });
    li.onmouseup = () => setSelection({ address: field.address, isLabel: false });

    return li;
  }

  function jumpTo(address, continuation) {
    if (isVisible({ address: address, isLabel: true })) {
      if (continuation) continuation();
      return;
    };
    httpGET(
      "disassembly?bank=" + address.bank.toString(16) +
      "&offset=" + address.offset.toString(16) +
      "&n=" + LINES,
      text => {
        const disassembly = JSON.parse(text);
        state.lines = disassembly.fields.slice(0, LINES).map(field => {
          return {
            address: field.address, field: field,
            li: createField(field, disassembly.breakpoints)
          };
        })

        setScrollIndex(0);
        refreshListing();
        setPC(state.pc);
        setSelection(state.selection);
        if (continuation) continuation();
      });
  }

  function moveSelection(amount) {
    const i = positionToIndex(state.selection);
    if (i < state.scrollIndex || i >= state.scrollIndex + LINES) {
      jumpTo(state.selection.address, () => moveSelection(amount));
    } else {
      const iNext = i + amount;
      if (iNext < state.scrollIndex) {
        scroll(iNext - state.scrollIndex, () =>
          setSelection(indexToPosition(state.scrollIndex)));
      } else if (iNext >= state.scrollIndex + LINES) {
        scroll(iNext - LINES - state.scrollIndex + 1, () =>
          setSelection(indexToPosition(state.scrollIndex + LINES - 1)));
      } else {
        setSelection(indexToPosition(iNext));
      }
    }
  }

  let isScrolling = false;
  let nextScrollAmount = 0;
  function scroll(amount, continuation) {
    if (!isScrolling) {
      doScroll(amount);
    } else {
      nextScrollAmount += amount;
    }

    function doNextScroll() {
      if (nextScrollAmount === 0) {
        isScrolling = false;
      } else {
        const total = nextScrollAmount;
        nextScrollAmount = 0;
        doScroll(total);
      }
    }

    function doScroll(amount) {
      isScrolling = true;
      const adjustedAmount = state.scrollIndex + amount;
      if (adjustedAmount >= 0 && adjustedAmount <= state.maxScrollIndex) {
        setScrollIndex(adjustedAmount);
        if (continuation) continuation();
        doNextScroll();
        return;
      }

      const baseAddress = (adjustedAmount < 0
        ? state.lines[0]
        : state.lines[state.lines.length - 1]).address;

      httpGET(
        "disassembly?bank=" + baseAddress.bank.toString(16) +
        "&offset=" + baseAddress.offset.toString(16) +
        "&n=" + adjustedAmount,
        text => {
          const disassembly = JSON.parse(text);
          const newFields = disassembly.fields.slice(1)
            .map(field => {
              return {
                address: field.address,
                field: field,
                li: createField(field, disassembly.breakpoints)
              };
            });

          const ul = document.getElementById('disassemblyList');
          if (adjustedAmount < 0) {
            newFields.reverse();
            updateLabels(newFields);
            state.lines = newFields.concat(state.lines);
            ul.prepend(...newFields.map(line => line.li));
            setScrollIndex(Math.max(0, newFields.length + adjustedAmount));

            let i0 = state.scrollIndex + 2 * LINES;
            if (i0 < state.lines.length && state.lines[i0 - 1].hasOwnProperty('label')) {
              i0 += 1;
            }
            state.lines.splice(i0).forEach(line => ul.removeChild(line.li));

          } else {
            updateLabels(newFields);
            state.lines = state.lines.concat(newFields);
            ul.append(...newFields.map(line => line.li));

            let toTrim = adjustedAmount;
            if (state.lines[toTrim - 1].hasOwnProperty('label')) {
              toTrim -= 1;
              setScrollIndex(1);
            } else {
              setScrollIndex(0);
            }

            state.lines.splice(0, toTrim).forEach(line => ul.removeChild(line.li));
          }

          updateMaxScrollIndex();
          setPC(state.pc);
          setSelection(state.selection);
          if (continuation) continuation()
          doNextScroll();
        },
        error => {
          isScrolling = false;
        }
      );
    }
  }

  function runToAddress(address) {
    httpPOST('/', 'runTo=&bank=' +
      address.bank.toString(16) + '&offset=' + address.offset.toString(16));
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
