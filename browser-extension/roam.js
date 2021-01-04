/**
 * Initialize the page action: set icon and title, then show.
 */
async function initializePageAction(tab) {
  const urlNoProtocol = tab.url.slice((new URL(tab.url)).protocol.length);
  const fetched = await fetch(`http://localhost:10001/roam/info?url=${urlNoProtocol}`);
  const body = await fetched.json();

  const pageExists = body.pageExists;

  const iconUrl = pageExists ? "org-roam-logo-active.svg" : "org-roam-logo-inactive.svg";
  const title = pageExists ? "org-roam page available" : "No org-roam page available";
  browser.pageAction.setIcon({ tabId: tab.id, path: iconUrl });
  browser.pageAction.setTitle({ tabId: tab.id, title });
  browser.pageAction.show(tab.id);
}

/**
 *  Each time a tab is updated, reset the page action for that tab.
 */
browser.tabs.onUpdated.addListener((id, changeInfo, tab) => {
  initializePageAction(tab);
});

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
