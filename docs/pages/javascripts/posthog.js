!function(t,e){var o,n,p,r;e.__SV||(window.posthog&&window.posthog.__loaded)||(window.posthog=e,e._i=[],e.init=function(i,s,a){function g(t,e){var o=e.split(".");2==o.length&&(t=t[o[0]],e=o[1]),t[e]=function(){t.push([e].concat(Array.prototype.slice.call(arguments,0)))}}(p=t.createElement("script")).type="text/javascript",p.crossOrigin="anonymous",p.async=!0,p.src=s.api_host.replace(".i.posthog.com","-assets.i.posthog.com")+"/static/array.js",(r=t.getElementsByTagName("script")[0]).parentNode.insertBefore(p,r);var u=e;for(void 0!==a?u=e[a]=[]:a="posthog",u.people=u.people||[],u.toString=function(t){var e="posthog";return"posthog"!==a&&(e+="."+a),t||(e+=" (stub)"),e},u.people.toString=function(){return u.toString(1)+".people (stub)"},o="Fi Di init Ji Xi Tr Ki tn Zi capture calculateEventProperties ln register register_once register_for_session unregister unregister_for_session dn getFeatureFlag getFeatureFlagPayload getFeatureFlagResult getAllFeatureFlags isFeatureEnabled reloadFeatureFlags updateFlags updateEarlyAccessFeatureEnrollment getEarlyAccessFeatures on onFeatureFlags onSurveysLoaded onSessionId getSurveys getActiveMatchingSurveys renderSurvey displaySurvey cancelPendingSurvey canRenderSurvey canRenderSurveyAsync cn identify setPersonProperties unsetPersonProperties group resetGroups setPersonPropertiesForFlags resetPersonPropertiesForFlags setGroupPropertiesForFlags resetGroupPropertiesForFlags reset shutdown setIdentity clearIdentity get_distinct_id getGroups get_session_id get_session_replay_url alias set_config startSessionRecording stopSessionRecording sessionRecordingStarted captureException addExceptionStep captureLog startExceptionAutocapture stopExceptionAutocapture loadToolbar get_property getSessionProperty un sn createPersonProfile setInternalOrTestUser hn Wi pn opt_in_capturing opt_out_capturing has_opted_in_capturing has_opted_out_capturing get_explicit_consent_status is_capturing clear_opt_in_out_capturing rn debug Er it getPageViewId captureTraceFeedback captureTraceMetric Ui".split(" "),n=0;n<o.length;n++)g(u,o[n]);e._i.push([i,s,a])},e.__SV=1)}(document,window.posthog||[]);

const POINTY_LEGACY_ANALYTICS_CONSENT_KEY="pointy.analytics-consent";

function getLegacyAnalyticsConsent(){
  try {
    return localStorage.getItem(POINTY_LEGACY_ANALYTICS_CONSENT_KEY);
  } catch {
    return null;
  }
}

function clearLegacyAnalyticsConsent(){
  try {
    localStorage.removeItem(POINTY_LEGACY_ANALYTICS_CONSENT_KEY);
  } catch {
    // PostHog remains the source of truth when browser storage is unavailable.
  }
}

function startPostHog(){
  posthog.init("phc_orj7Gcb85ofd5DmvvwhaH7eHHq6JyWT3SQyw4jycWMnN",{
    api_host:"/ingest",
    ui_host:"https://eu.posthog.com",
    cookieless_mode:"on_reject",
    defaults:"2026-05-30",
    person_profiles:"identified_only",
    loaded(client){
      const legacyConsent=getLegacyAnalyticsConsent();
      if(legacyConsent==="accepted"){
        client.opt_in_capturing();
        if(client.get_explicit_consent_status()==="granted"){
          clearLegacyAnalyticsConsent();
        }else{
          showCookieBanner();
        }
      }else if(legacyConsent==="declined"){
        client.opt_out_capturing();
        if(client.get_explicit_consent_status()==="denied"){
          clearLegacyAnalyticsConsent();
          showCookieSettings();
        }else{
          showCookieBanner();
        }
      }else if(client.get_explicit_consent_status()==="pending"){
        showCookieBanner();
      }else if(client.get_explicit_consent_status()==="denied"){
        showCookieSettings();
      }
    }
  });
}

function showCookieSettings(){
  if(document.querySelector(".cookie-settings")) return;

  const settings=document.createElement("button");
  settings.type="button";
  settings.className="cookie-settings";
  settings.textContent="Cookie settings";
  settings.addEventListener("click",()=>{
    settings.remove();
    showCookieBanner();
  });
  document.body.append(settings);
}

function showCookieBanner(){
  const banner=document.createElement("aside");
  banner.className="cookie-banner";
  banner.setAttribute("aria-label","Cookie preferences");

  const message=document.createElement("p");
  message.textContent="If you don't mind, please allow us to ask your browser to store an anonymous identifier so that we can connect the dots in case you visit again. It's perfectly fine to decline.";

  const actions=document.createElement("div");
  actions.className="cookie-banner__actions";

  const decline=document.createElement("button");
  decline.type="button";
  decline.className="cookie-banner__button cookie-banner__button--secondary";
  decline.textContent="Decline";

  const accept=document.createElement("button");
  accept.type="button";
  accept.className="cookie-banner__button";
  accept.textContent="Allow analytics";

  decline.addEventListener("click",()=>{
    posthog.opt_out_capturing();
    if(posthog.get_explicit_consent_status()!=="denied") return;
    banner.remove();
    showCookieSettings();
  });
  accept.addEventListener("click",()=>{
    posthog.opt_in_capturing();
    banner.remove();
  });

  actions.append(decline,accept);
  banner.append(message,actions);
  document.body.append(banner);
}

startPostHog();
