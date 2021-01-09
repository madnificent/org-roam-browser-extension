// [[file:../README.org::*Browser plugin implementation][Browser plugin implementation:1]]
const tabInfo = {};

/**
 * Initialize the page action: set icon and title, then show.
 */
async function initializePageAction(tab) {
  // Make information request over http
  const urlNoProtocol = tab.url.slice((new URL(tab.url)).protocol.length);
  const fetched = await fetch(`http://localhost:10001/roam/info?url=${urlNoProtocol}`);
  const body = await fetched.json();

  // Extract information from request
  const pageExists = body.pageExists;
  const linkExists = body.linkExists;
  const parentKnown = body.parentKnown;

  // set up information to be rendered for the icon
  let iconUrl;
  let title;
  let found;
  if (pageExists) {
    iconUrl = "org-roam-logo-has-page.svg";
    title = "Has page";
    found = true;
  } else if (linkExists) {
    iconUrl = "org-roam-logo-has-link.svg";
    title = "Has link";
    found = true;
  } else if (parentKnown) {
    iconUrl = "org-roam-logo-has-upper-reference.svg";
    title = "Parent is known";
    found = true;
  } else {
    iconUrl = "org-roam-logo-inactive.svg";
    title = "Nothing found";
    found = false;
  }

  if (found) {
    title += `: ${body.bestLink};`;
  }

  // attach information to the icon
  browser.pageAction.setIcon({ tabId: tab.id, path: iconUrl });
  browser.pageAction.setTitle({ tabId: tab.id, title });
  browser.pageAction.show(tab.id);

  // enable click handler
  tabInfo[tab.id] = { link: body.bestLink };

  browser.pageAction.onClicked.removeListener( clickEventListener );
  browser.pageAction.onClicked.addListener( clickEventListener );
}

async function clickEventListener(tab) {
  // the tab itself does not need to stay the same object, but the id
  // is stable.  If a browser creates an infinite amount of tab ids,
  // this would be a small memory leak.
  const link = tabInfo[tab.id]?.link;
  if( link ) {
    fetch(`http://localhost:10001/roam/open?page=${link}`);
  }
}
// Browser plugin implementation:1 ends here

// [[file:../README.org::*Browser plugin implementation][Browser plugin implementation:2]]
/**
 *  Each time a tab is updated, reset the page action for that tab.
 */
browser.tabs.onUpdated.addListener((id, changeInfo, tab) => {
  initializePageAction(tab);
});
// Browser plugin implementation:2 ends here

// [[file:../README.org::*Browser plugin implementation][Browser plugin implementation:3]]
/**
 * When first loaded, initialize the page action for all tabs.
 */
browser
  .tabs
  .query({})
  .then((tabs) => {
    for (let tab of tabs) {
      console.log("Initializing TAB");
      initializePageAction(tab);
    }
  });
// Browser plugin implementation:3 ends here
