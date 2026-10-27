(() => {
  'use strict';

  const STORAGE_KEY = 'regex-tracklayer';
  const MATCH_TIME_LIMIT_MS = 400;
  const VIBRATION_MS = 12;

  const app = Elm.Main.init({
    node: document.getElementById('app'),
    flags: {
      saved: readSaved(),
      coarsePointer: window.matchMedia('(pointer: coarse)').matches,
    },
  });

  function readSaved() {
    try {
      return localStorage.getItem(STORAGE_KEY);
    } catch (storageUnavailable) {
      return null;
    }
  }

  app.ports.save.subscribe((json) => {
    try {
      localStorage.setItem(STORAGE_KEY, json);
    } catch (storageUnavailable) {}
  });

  function findMatches(pattern, text) {
    const maxMatches = 300;
    const matches = [];
    try {
      const regex = new RegExp(pattern, 'g');
      let match;
      while (matches.length < maxMatches && (match = regex.exec(text))) {
        if (match[0] === '') {
          regex.lastIndex++;
        } else {
          matches.push([match.index, match[0].length]);
        }
      }
    } catch (invalidPattern) {}
    return matches;
  }

  const matcherWorkerUrl = URL.createObjectURL(
    new Blob(
      [`${findMatches}\nonmessage = (event) => postMessage(findMatches(event.data.pattern, event.data.text));`],
      { type: 'text/javascript' },
    ),
  );

  let runningMatcher = null;

  function stopRunningMatcher() {
    if (runningMatcher) {
      clearTimeout(runningMatcher.deadline);
      runningMatcher.worker.terminate();
      runningMatcher = null;
    }
  }

  function syntaxError(pattern) {
    try {
      new RegExp(pattern);
      return '';
    } catch (error) {
      return error.message;
    }
  }

  app.ports.checkPattern.subscribe(({ checkId, pattern, text }) => {
    const error = syntaxError(pattern);
    app.ports.patternChecked.send({ checkId, error });

    stopRunningMatcher();
    if (!text || !pattern || error) return;

    const report = (ranges, timedOut) => app.ports.matchesFound.send({ checkId, timedOut, ranges });
    try {
      const worker = new Worker(matcherWorkerUrl);
      const deadline = setTimeout(() => {
        stopRunningMatcher();
        report([], true);
      }, MATCH_TIME_LIMIT_MS);
      runningMatcher = { worker, deadline };
      worker.onmessage = (event) => {
        stopRunningMatcher();
        report(event.data, false);
      };
      worker.onerror = () => {
        stopRunningMatcher();
        report(findMatches(pattern, text), false);
      };
      worker.postMessage({ pattern, text });
    } catch (workersUnavailable) {
      report(findMatches(pattern, text), false);
    }
  });

  app.ports.copyText.subscribe(async (text) => {
    try {
      await navigator.clipboard.writeText(text);
    } catch (clipboardUnavailable) {
      const range = document.createRange();
      range.selectNodeContents(document.getElementById('out'));
      const selection = getSelection();
      selection.removeAllRanges();
      selection.addRange(range);
      document.execCommand('copy');
    }
  });

  function afterElmRenders(action) {
    requestAnimationFrame(action);
  }

  app.ports.selectText.subscribe((id) =>
    afterElmRenders(() => {
      const input = document.getElementById(id);
      if (input) {
        input.focus();
        input.select();
      }
    }),
  );

  app.ports.restoreFocus.subscribe((id) =>
    afterElmRenders(() => {
      const target = document.getElementById(id);
      const active = document.activeElement;
      const diagram = document.getElementById('rail');
      if (target) {
        if (active !== target) target.focus({ preventScroll: true });
      } else if (active && diagram && diagram.contains(active)) {
        active.blur();
      }
    }),
  );

  app.ports.vibrate.subscribe(() => {
    if (navigator.vibrate) navigator.vibrate(VIBRATION_MS);
  });
})();
