/* Timeline Extension JS */

(function () {

  /* ── Phase 4: Duplicate label grouping ────────────────────────
     Consecutive .event elements sharing the same data-label are
     wrapped in a .tl-group.event div that receives the shared dot
     and label. Inner events have their dot/label suppressed by CSS.
  ──────────────────────────────────────────────────────────────── */
  function groupDuplicateLabels(timeline) {
    const events = Array.from(timeline.querySelectorAll(':scope > .event'));
    if (events.length < 2) return;

    let i = 0;
    while (i < events.length) {
      const label = events[i].dataset.label;

      // Find run of consecutive events with the same label
      let j = i + 1;
      while (j < events.length && events[j].dataset.label === label) {
        j++;
      }

      if (j - i > 1 && label) {
        const group = document.createElement('div');
        group.classList.add('event', 'tl-group');
        if (label) group.dataset.label = label;

        // Insert wrapper before the first event in the run
        timeline.insertBefore(group, events[i]);

        // Move events into the wrapper
        for (let k = i; k < j; k++) {
          group.appendChild(events[k]);
        }
      }

      i = j;
    }
  }


  /* ── Phase 6: Fragment behavior ───────────────────────────────
     Two pan modes for timelines with more events than fit on a slide.
     Works with all layout variants (horizontal, vertical, snake, etc.).

     .fragment-slide   — centers the last visible event in the viewport
     .fragment-conveyor — pans just enough so the last visible event's
                          trailing edge is in view (steps only when
                          the next event would otherwise be clipped)

     In non-revealjs HTML, all .fragment classes are stripped so
     every event shows statically.
  ──────────────────────────────────────────────────────────────── */

  // Returns the last event that is either non-fragment or already visible.
  // Falls back to the first event if none qualify (e.g. before any fragment
  // has been revealed on the current slide).
  function getLastVisibleEvent(timeline) {
    const events = Array.from(timeline.querySelectorAll(':scope > .event'));
    let last = null;
    for (const el of events) {
      if (!el.classList.contains('fragment') || el.classList.contains('visible')) {
        last = el;
      }
    }
    return last || events[0] || null;
  }

  function isVerticalTimeline(timeline) {
    return timeline.classList.contains('vertical') ||
           timeline.classList.contains('vertical-right') ||
           timeline.classList.contains('vertical-alt') ||
           timeline.classList.contains('snake');
  }

  function panToCenter(timeline, targetEvent) {
    if (!targetEvent) return;
    if (isVerticalTimeline(timeline)) {
      const containerHeight = timeline.parentElement.offsetHeight;
      const eventCenter = targetEvent.offsetTop + targetEvent.offsetHeight / 2;
      const offset = containerHeight / 2 - eventCenter;
      timeline.style.transform = 'translateY(' + offset + 'px)';
    } else {
      const containerWidth = timeline.parentElement.offsetWidth;
      const eventCenter = targetEvent.offsetLeft + targetEvent.offsetWidth / 2;
      const offset = containerWidth / 2 - eventCenter;
      timeline.style.transform = 'translateX(' + offset + 'px)';
    }
  }

  function panConveyor(timeline, targetEvent) {
    if (!targetEvent) return;
    if (isVerticalTimeline(timeline)) {
      const containerHeight = timeline.parentElement.offsetHeight;
      const bottomEdge = targetEvent.offsetTop + targetEvent.offsetHeight;
      if (bottomEdge <= containerHeight) {
        // Event fits in view — no panning needed
        timeline.style.transform = 'translateY(0)';
      } else {
        // Pan just enough to show the bottom edge of the last visible event
        const offset = containerHeight - bottomEdge;
        timeline.style.transform = 'translateY(' + offset + 'px)';
      }
    } else {
      const containerWidth = timeline.parentElement.offsetWidth;
      const rightEdge = targetEvent.offsetLeft + targetEvent.offsetWidth;
      if (rightEdge <= containerWidth) {
        // Event fits in view — no panning needed
        timeline.style.transform = 'translateX(0)';
      } else {
        // Pan just enough to show the right edge of the last visible event
        const offset = containerWidth - rightEdge;
        timeline.style.transform = 'translateX(' + offset + 'px)';
      }
    }
  }

  function updatePan(timeline) {
    const target = getLastVisibleEvent(timeline);
    if (!target) return;
    if (timeline.classList.contains('fragment-slide')) {
      panToCenter(timeline, target);
    } else if (timeline.classList.contains('fragment-conveyor')) {
      panConveyor(timeline, target);
    }
  }

  function initFragments() {
    const isReveal = typeof window.Reveal !== 'undefined';

    if (!isReveal) {
      // Strip all fragment classes so every event shows statically
      document.querySelectorAll('.timeline .fragment').forEach(function (el) {
        el.classList.remove('fragment', 'visible', 'current-fragment');
      });
      return;
    }

    // React to fragment transitions
    Reveal.on('fragmentshown', function (event) {
      const timeline = event.fragment.closest('.timeline');
      if (timeline) updatePan(timeline);
    });

    Reveal.on('fragmenthidden', function (event) {
      const timeline = event.fragment.closest('.timeline');
      if (timeline) updatePan(timeline);
    });

    // Initialize pan position when a slide becomes active
    Reveal.on('slidechanged', function (event) {
      if (event.currentSlide) {
        event.currentSlide
          .querySelectorAll('.timeline.fragment-slide, .timeline.fragment-conveyor')
          .forEach(updatePan);
      }
    });

    // Initialize pan position for the opening slide
    function onReady() {
      const current = Reveal.getCurrentSlide();
      if (current) {
        current
          .querySelectorAll('.timeline.fragment-slide, .timeline.fragment-conveyor')
          .forEach(updatePan);
      }
    }

    if (Reveal.isReady()) {
      onReady();
    } else {
      Reveal.on('ready', onReady);
    }
  }


  /* ── Phase 5: Mark empty events ───────────────────────────────
     CSS :empty fails when Pandoc emits a newline or <p></p> inside
     the div. textContent.trim() covers all those cases.
  ──────────────────────────────────────────────────────────────── */
  function markEmptyEvents(timeline) {
    timeline.querySelectorAll(':scope > .event').forEach(function (el) {
      if (el.textContent.trim() === '') {
        el.classList.add('tl-empty');
      }
    });
  }


  /* ── Init ──────────────────────────────────────────────────── */
  document.addEventListener('DOMContentLoaded', function () {
    document.querySelectorAll('.timeline').forEach(function (tl) {
      markEmptyEvents(tl);
      groupDuplicateLabels(tl);
    });
    initFragments();
  });

})();
