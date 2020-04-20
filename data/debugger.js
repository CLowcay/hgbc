'use strict';

window.onload = () => {
  const eventSource = new EventSource('events');
  window.onbeforeunload = () => eventSource.close();

  const banks = new BankStatus();
  const memory = new Memory();
  const disassembly = new Disassembly(banks);
  initStatus(eventSource, banks, memory, disassembly);

  eventSource.addEventListener('breakpoint-added',
    event => disassembly.breakPointAdded(JSON.parse(event.data)));
  eventSource.addEventListener('breakpoint-removed',
    event => disassembly.breakPointRemoved(JSON.parse(event.data)));
  eventSource.addEventListener('label-added',
    event => disassembly.labelUpdated(JSON.parse(event.data)));
  eventSource.addEventListener('label-removed',
    event => disassembly.labelRemoved(JSON.parse(event.data)));

  document.getElementById('run').onclick = () => postCommand('/', 'run');
  document.getElementById('step').onclick = () => postCommand('/', 'step');
  document.getElementById('stepOver').onclick = () => postCommand('/', 'stepOver');
  document.getElementById('stepOut').onclick = () => postCommand('/', 'stepOut');
  document.getElementById('restart').onclick = () => postCommand('/', 'restart');

  const globalKeymap = new KeyMap();
  document.addEventListener('keydown', globalKeymap.handleEvent);

  globalKeymap.override('control KeyG', () =>
    document.getElementById('disassemblyAddress').focus());
  globalKeymap.override('control KeyI', () => postCommand('/', 'step'));
  globalKeymap.override('control KeyO', () => postCommand('/', 'stepOut'));
  globalKeymap.override('control KeyP', () => postCommand('/', 'stepOver'));
  globalKeymap.override('control shift KeyR', () => postCommand('/', 'restart'));
  globalKeymap.override('Pause', () => postCommand('/', 'run'));
  globalKeymap.override('control KeyR', () => postCommand('/', 'run'));
  globalKeymap.override('control Home', () => disassembly.revealPC());
};

/*****************************************************************************
 * BANK STATUS
 *****************************************************************************/
function BankStatus() {
  const banks = { pcBank: 0, rom: 0, ram: 0, vram: 0, wram: 0 };

  this.update = function (status) {
    banks.pcBank = parseInt(status.pcBank, 16);
    banks.rom = parseInt(status.romBank, 16);
    banks.ram = parseInt(status.ramBank, 16);
    banks.vram = parseInt(status.vbk0, 16);
    banks.wram = parseInt(status.svbk2_0, 16);
  }

  this.expandAddress = function (offset, relativeTo) {
    if (offset < 0x4000) {
      if (relativeTo.bank === 0xFFFF &&
        (offset < 0x100 || (offset >= 0x200 && offset < 0x1000))) {
        return { bank: 0xFFFF, offset: offset };
      } else {
        return { bank: 0, offset: offset };
      }
    } else if (offset < 0x8000) {
      return relativeTo.offset >= 0x4000 && relativeTo.offset < 0x8000 ?
        { bank: relativeTo.bank, offset: offset } :
        { bank: banks.rom, offset: offset };
    } else if (offset < 0xA000) {
      return relativeTo.offset >= 0x8000 && relativeTo.offset < 0xA000 ?
        { bank: relativeTo.bank, offset: offset } :
        { bank: banks.vram, offset: offset };
    } else if (offset < 0xC000) {
      return relativeTo.offset >= 0xA000 && relativeTo.offset < 0xC000 ?
        { bank: relativeTo.bank, offset: offset } :
        { bank: banks.ram, offset: offset };
    } else if (offset < 0xD000) {
      return { bank: 0, offset: offset };
    } else if (offset < 0xE000) {
      return relativeTo.offset >= 0xD000 && relativeTo.offset < 0xE000 ?
        { bank: relativeTo.bank, offset: offset } :
        { bank: banks.wram, offset: offset };
    } else if (offset < 0xF000) {
      return { bank: 0, offset: offset };
    } else if (offset < 0xFE00) {
      return relativeTo.offset >= 0xF000 && relativeTo.offset < 0xFE00 ?
        { bank: relativeTo.bank, offset: offset } :
        { bank: banks.wram, offset: offset };
    } else {
      return { bank: 0, offset: offset };
    }
  }
}

/*****************************************************************************
 * STATUS
 *****************************************************************************/
function initStatus(eventSource, banks, memory, disassembly) {
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
    banks.update(data);
    for (let key of Object.keys(data)) {
      if (key === 'pcBank') continue;
      const value = data[key];
      const element = document.getElementById(key);
      if (!element) console.log(`bad key ${key}`);
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

  async function refresh(baseAddress) {
    const text = await fetchText("memory?address=" + baseAddress.toString(16) + "&lines=" + LINES);
    document.getElementById('memoryHex').innerText = text;
    document.getElementById('memoryASCII').innerText =
      text.split('\n')
        .map(line => line.split(' ')
          .map(c => decodeASCII(parseInt(c, 16)))
          .join(''))
        .join('\n');
  }
}

/*****************************************************************************
 * Disassembly
 *****************************************************************************/
function Disassembly(banks) {
  const LINES = 20;
  const LINE_HEIGHT = 1.5;

  const state = {
    lines: [], // { address, field :: { text, data } | label :: string, li :: Element<li> }
    scrollIndex: 0,
    maxScrollIndex: LINES,
    pc: { bank: 0, offset: 0 },
    selection: { isLabel: false, address: { bank: 0, offset: 0 } },
    labels: new LongAddressMap()
  };

  let newestLabel = undefined;

  this.setPC = setPC;
  this.revealPC = async () => {
    await jumpTo(state.pc);
    setSelection({ address: state.pc, isLabel: false });
  };

  this.breakPointAdded = function (address) {
    const li = getFieldLI(address);
    if (li) li.querySelector('input.breakpoint').classList.add('set');
  };
  this.breakPointRemoved = function (address) {
    const li = getFieldLI(address);
    if (li) li.querySelector('input.breakpoint').classList.remove('set');
  };
  this.labelUpdated = function (labels) {
    labels.forEach(label => state.labels.set(label.address, label.text));
    refreshLabels();
    refreshListing();
  };
  this.labelRemoved = function (address) {
    state.labels.delete(address);
    refreshLabels();
    refreshListing();
  };

  // Set up the disassembler buttons.
  document.getElementById('toPC').onclick = () => {
    jumpTo(state.pc).then(() => setSelection({ address: state.pc, isLabel: false }));
  }
  document.getElementById('runTo').onclick = () => runToAddress(state.selection.address);
  document.getElementById('breakpoint').onclick = () =>
    jumpTo(state.selection.address).then(() =>
      toggleBreakpointAtVisibleAddress(state.selection.address));
  document.getElementById('label').onclick = async () => {
    const address = state.selection.address;
    await jumpTo(address);
    if (!newLabelAt(address)) {
      const position = { address: address, isLabel: true };
      const text = getLI(position).firstChild;
      setSelection(position);
      window.getSelection().setBaseAndExtent(text, 0, text, 1);
    }
  }

  const disassemblyKeymap = new KeyMap();
  const disassemblyWindow = document.querySelector('div.disassembly div.window');
  disassemblyWindow.addEventListener('wheel', onWheel);
  disassemblyWindow.addEventListener('keydown', disassemblyKeymap.handleEvent);

  disassemblyKeymap.override('ArrowUp', () => moveSelection(-1));
  disassemblyKeymap.override('ArrowDown', () => moveSelection(1));
  disassemblyKeymap.override('PageUp', () => moveSelection(-LINES));
  disassemblyKeymap.override('PageDown', () => moveSelection(LINES));
  disassemblyKeymap.override('Enter', () => moveSelection(1));
  disassemblyKeymap.override('NumpadEnter', () => moveSelection(1));
  disassemblyKeymap.add('shift Equal', () =>
    jumpTo(state.selection.address).then(() => newLabelAt(state.selection.address)));
  disassemblyKeymap.add('NumpadAdd', () =>
    jumpTo(state.selection.address).then(() => newLabelAt(state.selection.address)));
  disassemblyKeymap.add('Home', handleFocusSelection);
  disassemblyKeymap.add('End', handleFocusSelection);
  disassemblyKeymap.add('ArrowLeft', handleFocusSelection);
  disassemblyKeymap.add('ArrowRight', handleFocusSelection);
  disassemblyKeymap.add('Space', handleFocusSelection);
  disassemblyKeymap.override('control KeyH', () => runToAddress(state.selection.address));
  disassemblyKeymap.override('control KeyB', () =>
    jumpTo(state.selection.address).then(() =>
      toggleBreakpointAtVisibleAddress(state.selection.address)));

  const disassemblyAddress = document.getElementById('disassemblyAddress');
  disassemblyAddress.addEventListener('blur', () => refreshDisassemblyAddress());
  disassemblyAddress.addEventListener('input', () => {
    const address = parseLongAddress(state.lines[0].address, disassemblyAddress.value);
    jumpTo(address).then(() => setSelection({ address: address, isLabel: false }));
  });

  const disassemblyAddressKeymap = new KeyMap();
  disassemblyAddress.addEventListener('keydown', disassemblyAddressKeymap.handleEvent);

  disassemblyAddressKeymap.override('ArrowUp', () => {
    disassemblyWindow.focus();
    moveSelection(-1);
  });
  disassemblyAddressKeymap.override('ArrowDown', () => {
    disassemblyWindow.focus();
    moveSelection(1);
  });
  disassemblyAddressKeymap.override('Enter', () => {
    disassemblyWindow.focus();
    moveSelection(0);
  });
  disassemblyAddressKeymap.override('NumpadEnter', () => {
    disassemblyWindow.focus();
    moveSelection(0);
  });

  // initialize the labels
  fetchJSON('labels').then(labels => {
    labels.forEach(label => state.labels.set(label.address, label.text));
    refreshListing();
  });

  function newLabelAt(address) {
    if (!state.labels.has(address)) {
      newestLabel = address;
      postLabelUpdate(address, "newLabel");
      return true;
    } else {
      return false;
    }
  }

  async function handleFocusSelection(event) {
    if (!event.target.classList.contains('label')) {
      event.preventDefault();
      await moveSelection(0);
      updateFocus();
    }
  }

  function postLabelUpdate(address, text) {
    const uri = "label?bank=" + address.bank.toString(16) +
      "&offset=" + address.offset.toString(16);
    postCommand(uri, "update=" + encodeURIComponent(text));
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
    return state.lines[positionToIndex(position)];
  }

  function positionToIndex(position) {
    const r = state.lines.findIndex(line =>
      (sameAddress(line.address, position.address)) &&
      (line.hasOwnProperty('label') ? position.isLabel : true));

    return r !== -1 ? r : state.lines.findIndex(line =>
      (line.field ? addressInField(position.address, line.field) :
        sameAddress(line.address, position.address)) &&
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
      postCommand(uri, 'unset');
    } else {
      postCommand(uri, 'set');
    }
  }

  function updateLabels(lines) {
    let i = 0;
    while (i < lines.length) {
      const line = lines[i];
      const label = state.labels.get(line.address);
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

  function refreshLabels() {
    state.lines.forEach(line => {
      if (line.hasOwnProperty('field') && line.field.text !== 'db') {
        const instruction = line.li.querySelector('.instruction');
        instruction.innerText = '';
        setInstructionContent(instruction, line.field);
      }
    });
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

    const keymap = new KeyMap();
    keymap.override('End', () => window.getSelection().collapse(text, 1));
    keymap.override('shift End', () => window.getSelection().extend(text, 1));

    text.addEventListener('keydown', keymap.handleEvent);
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
      setInstructionContent(instruction, field);

      const bytes = document.createElement('span');
      bytes.classList.add('bytes');
      bytes.innerText = '; ' + field.bytes

      li.appendChild(instruction);
      li.appendChild(bytes);
    }

    if (field.overlap) {
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

  function setInstructionContent(instruction, field) {
    if (field.overlap) instruction.append('; ');
    instruction.append(field.text);
    let first = true;
    const parameters = field.p.map(formatParameter);
    for (const parameter of parameters) {
      instruction.append(first ? ' ' : ', ');
      instruction.append(parameter);
      first = false;
    }
    instruction.append("\t");

    function formatParameter(parameter) {
      if (!parameter.hasOwnProperty('address')) return parameter.text;

      const address = banks.expandAddress(parameter.address, field.address);
      const label = state.labels.get(address);
      if (!label) return parameter.text;

      const addressText = formatLongAddress(address);
      const a = document.createElement('a');
      a.href = '#' + encodeURIComponent(addressText);
      a.innerText = parameter.indirect ? `(${label})` : label;
      a.addEventListener('click', () => jumpTo(address).then(() =>
        setSelection({ address: address, isLabel: false })));
      a.addEventListener('keydown', event => {
        if (event.key === 'Enter') event.stopPropagation();
      });

      const info = document.createElement('div');
      info.classList.add('info');
      info.innerText = addressText;

      const wrapper = document.createElement('span');
      wrapper.style.position = 'relative';
      wrapper.appendChild(a);
      wrapper.appendChild(info);
      return wrapper;
    }
  }

  async function jumpTo(address) {
    if (isVisible({ address: address, isLabel: true })) return;

    const disassembly = await fetchJSON(
      "disassembly?bank=" + address.bank.toString(16) +
      "&offset=" + address.offset.toString(16) +
      "&n=" + LINES);

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
  };

  async function moveSelection(amount) {
    const i = positionToIndex(state.selection);
    if (i < state.scrollIndex || i >= state.scrollIndex + LINES) {
      await jumpTo(state.selection.address);
      await moveSelection(amount);
    } else {
      const iNext = i + amount;
      if (iNext < state.scrollIndex) {
        await scroll(iNext - state.scrollIndex);
        setSelection(indexToPosition(state.scrollIndex));
      } else if (iNext >= state.scrollIndex + LINES) {
        await scroll(iNext - LINES - state.scrollIndex + 1);
        setSelection(indexToPosition(state.scrollIndex + LINES - 1));
      } else {
        setSelection(indexToPosition(iNext));
      }
    }
  }

  let isScrolling = false;
  let nextScrollAmount = 0;
  async function scroll(amount0) {
    nextScrollAmount += amount0;
    if (isScrolling) return;

    isScrolling = true;
    try {
      while (nextScrollAmount !== 0) {
        const amount = nextScrollAmount;
        nextScrollAmount = 0;
        await doScroll(amount);
      }
    } finally {
      isScrolling = false;
    }

    async function doScroll(amount) {
      const adjustedAmount = state.scrollIndex + amount;
      if (adjustedAmount >= 0 && adjustedAmount <= state.maxScrollIndex) {
        setScrollIndex(adjustedAmount); return;
      }

      const baseAddress = (adjustedAmount < 0
        ? state.lines[0]
        : state.lines[state.lines.length - 1]).address;

      const disassembly = await fetchJSON(
        "disassembly?bank=" + baseAddress.bank.toString(16) +
        "&offset=" + baseAddress.offset.toString(16) +
        "&n=" + adjustedAmount
      );

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
    }
  };

  function runToAddress(address) {
    postCommand('/', 'runTo=&bank=' +
      address.bank.toString(16) + '&offset=' + address.offset.toString(16));
  }
}

/*****************************************************************************
 * Utility
 *****************************************************************************/
function sameAddress(a, b) {
  return a.offset === b.offset && a.bank === b.bank;
}

function addressInField(address, field) {
  return address.bank === field.address.bank &&
    address.offset >= field.address.offset &&
    address.offset < field.address.offset + ((field.bytes.length + 1) / 3);
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

function LongAddressMap() {
  const map = new Map();
  this.set = (key, value) => map.set(makeKey(key), value);
  this.delete = key => map.delete(makeKey(key));
  this.get = key => map.get(makeKey(key));
  this.has = key => map.has(makeKey(key))
  this.size = () => map.size();

  function makeKey(address) {
    return address.bank << 16 | address.offset;
  }
}

function KeyMap() {
  const map = {};

  this.add = function (key, action) {
    map[makeKey(key)] = action;
  };

  this.override = function (key, action) {
    map[makeKey(key)] = event => {
      event.preventDefault();
      action(event);
    };
  };

  this.handleEvent = function (event) {
    const mods = modifiersFromEvent(event);
    let action = map[event.code + mods];
    if (action) {
      return action(event);
    } else {
      action = map[event.code];
      if (action) return action(event);
    }
  }

  function makeKey(key) {
    const elements = key.split(' ');
    const modifiers = elements.splice(0, elements.length - 1).map(mod => {
      switch (mod) {
        case 'control': return 1;
        case 'alt': return 2;
        case 'shift': return 4;
      }
    });
    if (modifiers.length === 0) {
      return elements[0];
    } else {
      return elements[0] + modifiers.reduce((a, i) => a | i, 0);
    }
  }

  function modifiersFromEvent(event) {
    return (event.ctrlKey ? 1 : 0) |
      (event.altKey ? 2 : 0) |
      (event.shiftKey ? 4 : 0);
  }
}

async function fetchText(...args) {
  const response = await fetch(...args);
  if (!response.ok) {
    throw new Error(`Fetch failed with ${response.status}`);
  } else {
    return response.text();
  }
}

async function fetchJSON(...args) {
  const response = await fetch(...args);
  if (!response.ok) {
    throw new Error(`Fetch failed with ${response.status}`);
  } else {
    return response.json();
  }
}

function postCommand(uri, command) {
  fetch(uri, {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: command
  });
}
