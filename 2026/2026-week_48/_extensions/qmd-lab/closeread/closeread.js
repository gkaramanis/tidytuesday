//==============//
// closeread.js //
//==============//

// set params
const triggerSelector = '.new-trigger'
const progressBlockSelector = '.progress-block'


//=================//
// Event Listeners //
//=================//
/* the interactive nature of closeread docs is enabled by event listeners that
   execute code when the user loads the doc, scrolls, presses keys, etc. 
*/

// == Run upon the HTML file loaded === //
document.addEventListener("DOMContentLoaded", () => {

  // attach meta classes to <body>
  document.body.classList.add("closeread")
  const debugMode         = getBooleanConfig("cr-debug-mode")
  const removeHeaderSpace = getBooleanConfig("cr-remove-header-space")
  if (debugMode) {
    document.body.classList.add("cr-debug")
  } 
  if (removeHeaderSpace) {
    document.body.classList.add("cr-removeheaderspace")
  }

  // attach layout classes to parents of `.cr-section`s up to main.content
  /* this replicates quarto <= 1.6 functionality:
    https://github.com/quarto-dev/quarto-cli/blob/
      d85467627aae71c96e3d1e9718a3b47289329cde/src/format/html/
      format-html-bootstrap.ts#L1163C1-L1186C7 */
  const ensureInGrid = el => {
    const parent = el.parentElement
    parent.classList.add("page-columns", "page-full")
    if (isDocumentMain(parent)) {
      return
    } else {
      ensureInGrid(parent)
    }
  }  
  const crSections = Array.from(document.querySelectorAll(".cr-section"))
  crSections.map(ensureInGrid)

  const ojsModule = window._ojs?.ojsConnector?.mainModule
  const ojsStickyName = ojsModule?.variable()
  const ojsTriggerIndex = ojsModule?.variable()
  const ojsTriggerProgress = ojsModule?.variable()
  const ojsDirection = ojsModule?.variable()
  const ojsProgressBlock = ojsModule?.variable()

  let focusedSticky = "none";
  ojsStickyName?.define("crActiveSticky", focusedSticky);
  ojsTriggerIndex?.define("crTriggerIndex", 0);
  ojsTriggerProgress?.define("crTriggerProgress", 0);
  ojsDirection?.define("crDirection", null);
  ojsProgressBlock?.define("crProgressBlock", 0);

  if (ojsModule === undefined) {
    console.error("Warning: Quarto OJS module not found")
  }
  
  // expand hlz option into highlight and zoom-to
  const allHlzTriggers = Array.from(document.querySelectorAll('[data-hlz]'));
  allHlzTriggers.forEach(trigger => {
    const hlzValue = trigger.getAttribute('data-hlz');
    trigger.setAttribute('data-zoom-to', hlzValue);
    trigger.setAttribute('data-highlight', hlzValue);
  });
    
  // collect all sticky elements
  const allStickies = Array.from(document.querySelectorAll(".sticky"));
  
  // === Set up scrolling event listeners === //
  // scrollama() is accessible because scrollama.min.js is attached
  // via closeread.lua
  const triggerScrollerConfig = {
    step: triggerSelector,
    offset: 0.5,
    progress: true,
    debug: debugMode
  }
  const progressScrollerConfig = {
    step: progressBlockSelector,
    offset: 0.5,
    progress: true,
    debug: debugMode
  }

  function crTriggerStepEnter(trigger) {
    focusedStickyName = trigger.element.getAttribute("data-focus-on")
        
    // update ojs variables
    ojsTriggerIndex?.define("crTriggerIndex", trigger.index)
    ojsStickyName?.define("crActiveSticky", focusedStickyName)
      
    updateStickies(allStickies, focusedStickyName, trigger)
  }
  
  function crTriggerStepProgress(trigger) {
    ojsTriggerProgress?.define("crTriggerProgress", trigger.progress)
    ojsDirection?.define("crDirection", trigger.direction)
  }
  
  function crProgressStepEnter(progressBlock) {
    ojsProgressBlock?.define("crProgressBlock", progressBlock.progress)
  }
  
  // set up scrollers on document load, and reset them when window zoom changes
  // (they seem to misbehave on zoom change: see issue #101)

  // primary scroller
  const triggerScroller = scrollama()
  triggerScroller
    .setup(triggerScrollerConfig)
    .onStepEnter(crTriggerStepEnter)
    .onStepProgress(crTriggerStepProgress)
    
  // secondary scroller used for making progress blocks
  const progressBlockScroller = scrollama()
  progressBlockScroller
    .setup(progressScrollerConfig)
    .onStepProgress(crProgressStepEnter)

  window.addEventListener("resize", (event) => {
    setTimeout(() => triggerScroller.resize(), 1000)
    setTimeout(() => progressBlockScroller.resize(), 1000)
  })

  // === Hotkey Listeners === //
  // Add a listener for scrolling between new triggers
  let currentIndex = -1; // Start before the first element
  
  function scrollToNewTrigger(direction) {
    const triggers = document.querySelectorAll('.new-trigger');
    
    if (triggers.length === 0) return; // do nothing if there's no triggers
    
    if (direction === "next") {
      if (currentIndex >= triggers.length - 1) return; // exit if at end
      currentIndex += 1;
    }
    
    if (direction === "previous") {
      if (currentIndex === 0) return; // exit if at start
      currentIndex -= 1;
    }
    
    const nextTrigger = triggers[currentIndex];
    nextTrigger.scrollIntoView({ behavior: 'smooth', block: 'center' });
  }
  
  document.addEventListener('keydown', (event) => {
    if (event.key === 'ArrowRight') {
        scrollToNewTrigger("next");
    }
    if (event.key === 'ArrowLeft') {
        scrollToNewTrigger("previous");
    }
  });

  // toggle presentation mode
  document.addEventListener('keydown', (event) => {
    const crSections = document.querySelectorAll('.cr-section');
    crSections.forEach((el) => {
      if (event.key === "p") {
        if (el.classList.contains("presentation-mode")) {
            el.classList.remove("presentation-mode");
        } else {
            el.classList.add("presentation-mode");
        }
      }
    });
  });

 });
 
 
//===============//
// Focus effects //
//===============//
// A collection of functions that apply focus effects to stickies
 
// updateStickies: triggers effects on the focused sticky 
function updateStickies(allStickies, focusedStickyName, trigger) {
  const focusedSticky = document.querySelectorAll("[id=" + focusedStickyName)[0];
  
  // update which sticky is active
  allStickies.forEach(node => {node.classList.remove("cr-active")});
  focusedSticky.classList.add("cr-active");
        
  // apply additional effects
  transformSticky(focusedSticky, trigger.element);
  highlightSpans(focusedSticky, trigger.element);
  
  if ( // scale-to-fill only takes effect if there are no other transforms
    focusedSticky.classList.contains("scale-to-fill") &&
    !trigger.element.hasAttribute("data-zoom-to") &&
    !trigger.element.hasAttribute("data-pan-to") &&
    !trigger.element.hasAttribute("data-scale-by")
  ) {
    scaleToFill(focusedSticky);
  }

}


//==============//
// Highlighting //
//==============//

// highlights line number spans in line blocks and code, and id'ed spans in line blocks
function highlightSpans(focusedSticky, triggerEl) {
  
  // remove any previous highlighting
  focusedSticky.querySelectorAll("span[id]").forEach(d => d.classList.remove("cr-hl"));
  focusedSticky.classList.remove("cr-hl-within");
  
  // get hightlighted spans from trigger
  let highlightIds = triggerEl.getAttribute("data-highlight");
  
  // exit function if there's no highlighting
  if (highlightIds === null) {
    return;
  }
  
  // dim enclosing block
  focusedSticky.classList.add("cr-hl-within");
  
  // add highlight class to appropriate spans
  highlightIds = rangeToSeries(highlightIds);
  highlightIds.split(',').forEach(highlightId => {
    
    // build selector
    const spanSelector = idToSpanSelector(focusedSticky, highlightId);
    // find span
    const highlightSpan = focusedSticky.querySelector(spanSelector);
    
    // apply effect
    if (highlightSpan !== null) {
      highlightSpan.classList.add("cr-hl");
    } else {
    // Handle the case where the ID does not correspond to a span
      console.warn(`While highlighting, could not find span with corresponding to an ID of '${highlightId}'. Please ensure the ID is correct.`);
    }
  });
  
}

// turn a range of line numbers into a series
function rangeToSeries(ids) {
  isRange = /\b(\d+)\s*-\s*(\d+)\b/;
  if (isRange.test(ids)) {
    const match = ids.match(isRange);
  
    if (match) {
      const start = parseInt(match[1], 10);
      const end = parseInt(match[2], 10);
      const numbers = [];

      for (let i = start; i <= end; i++) {
        numbers.push(i);
      }

      ids = numbers.join(',');
    }
  }
  
  return ids;
}

// convert id to appropriate span selector
function idToSpanSelector(focusedSticky, id) {
  id.trim();
  let spanSelector = "";
  
  // determine the right spanSelector
  // for line numbers
  if (!isNaN(id)) {
    
    // that are in line blocks
    if (focusedSticky.querySelector('.line-block') !== null) {
      spanSelector = `span[id^="lb"][id*="-${id}"]`;
    }
    // or in code cells
    if (focusedSticky.querySelector('.cell') !== null || focusedSticky.querySelector('.sourceCode') !== null) {
      spanSelector = `span[id^="cb"][id*="-${id}"]`;
    }
    
  // and for span ids
  } else {
    spanSelector = `span[id="${id}"]`;
  }
  
  return spanSelector;
}


//==============//
// Transforming //
//==============//
// use the flexible `transform` attribute to trigger effects associated with
// zoom-to, pan-to, scale-by, .scale-to-fill, and (indirectly) hlz

function transformSticky(focusedSticky, trigger) {
  
  // initialize empty strings
  let translateStr = "";
  let scaleStr = "";
  let transformStr = "";
  
  // determine type of transform
  if (trigger.hasAttribute("data-pan-to")) {
    // get translate attributes from trigger
    translateStr = "translate(" + trigger.getAttribute("data-pan-to") + ")";
  }
  
  if (trigger.hasAttribute("data-scale-by")) {
    // get scale attributes from trigger
    scaleStr = "scale(" + trigger.getAttribute("data-scale-by") + ")";
  }
  
  if (trigger.hasAttribute("data-zoom-to")) {
    transformStr = zoomToTransform(focusedSticky, trigger);
  }
  
  // zooming will override pan-to and scale-by
  if (!transformStr) {
    if (translateStr && scaleStr) {
      transformStr = translateStr + " " + scaleStr;
    } else if (translateStr) {
      transformStr = translateStr;
    } else if (scaleStr) {
      transformStr = scaleStr;
    }
  }

  // use the string to transform the sticky
  focusedSticky.style.transform = transformStr;
  
}

function zoomToTransform(focusedSticky, trigger) {
  
  const paddingX = 75;
  const paddingY = 50;
  
  // get zoom-to spans from trigger
  let zoomToIds = trigger.getAttribute("data-zoom-to");
  zoomToIds = rangeToSeries(zoomToIds);
  
  // for now, exit function if user provides more than one span / line
  const zoomToArray = zoomToIds.split(',');
  if (zoomToArray.length > 1) {
    console.warn(`zoom-to currently only supports a single line number or span id.`);
    return;
  }
  
  // build selector
  const spanSelector = idToSpanSelector(focusedSticky, zoomToIds);
  // find span
  const focusedSpan = focusedSticky.querySelector(spanSelector);
  
  // measurements needed for translation
  const focusedStickyHeight = focusedSticky.scrollHeight
  const focusedStickyWidth = focusedSticky.scrollWidth
  const focusHeight = focusedSpan.offsetHeight
  focusedSpan.offsetTop;
  const focusTop = focusedSpan.offsetTop
  const focusCenterY = focusTop + (focusHeight / 2)
  const centerDeltaY = (focusCenterY - (focusedSticky.offsetHeight / 2)) * -1
  
  // measurements needed for scaling
  const container = focusedSticky.closest(".sticky-col-stack")
  const containerHeight = container.offsetHeight - (paddingY * 2)
  const containerWidth = container.offsetWidth - (paddingX * 2)
  // note scaleWidth uses the whole line, not just the span width
  const scaleWidth = focusedStickyWidth / containerWidth
  const scaleHeight = focusHeight / containerHeight
  const scaleFactor = 1 / Math.max(scaleHeight, scaleWidth)
  
  // form matrix transform string
  const transformStr = `matrix(${scaleFactor}, 0, 0, ${scaleFactor}, 0, ${centerDeltaY})`;
  
  return transformStr
}

// given an element `el`, rescales it to fill its containing .sticky-col-stack 
function scaleToFill(el, paddingX = 75, paddingY = 50) {

  // get dimensions of element and its container
  const container = el.closest(".sticky-col-stack")
  
  const elHeight = el.scrollHeight
  const elWidth = el.scrollWidth
  const containerHeight = container.offsetHeight - (paddingY * 2)
  const containerWidth = container.offsetWidth - (paddingX * 2)

  const scaleHeight = elHeight / containerHeight
  const scaleWidth = elWidth / containerWidth
  const scale = 1 / Math.max(scaleHeight, scaleWidth)
  
  const centerDeltaY = (elHeight - el.offsetHeight) * scale / -2

  // apply styles
  el.style.setProperty("transform",
    `matrix(${scale}, 0, 0, ${scale}, 0, ${centerDeltaY})`)
}

/* getBooleanConfig: checks for a <meta> with named attribute `cr-[metaFlag]`
   and returns true if its value is "true" or false otherwise */
function getBooleanConfig(metaFlag) {
  const option = document
    .querySelector("meta[" + metaFlag + "]")?.getAttribute(metaFlag)
  return option === "true"
}

function isDocumentMain(el) {
  return el === null ||
      (el.tagName == "MAIN" && el.classList.contains("content"))
}
