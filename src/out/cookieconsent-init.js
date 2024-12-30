var cc = initCookieConsent();

var cookie = '🍪';

// run plugin with config object
cc.run({
    current_lang : '#current_lang#',
    autoclear_cookies : true,                   // default: false
    cookie_name: '#cookie_name#',             // default: 'cc_cookie'
    cookie_expiration : #cookie_expiration#,                    // default: 182
    page_scripts: true,                         // default: false
    revision: #revision#,                       // default: 0

    auto_language: browser,                     // default: null; could also be 'browser' or 'document'
    // autorun: true,                           // default: true
    delay: #delay#,                                // default: 0
    force_consent: true,
    // hide_from_bots: false,                   // default: false
    remove_cookie_tables: #remove_cookie_tables#,              // default: false
    // cookie_domain: location.hostname,        // default: current domain
    // cookie_path: "/",                        // default: root
    // cookie_same_site: "Lax",
    // use_rfc_cookie: false,                   // default: false
    // revision: 0,                             // default: 0

    gui_options: {
        consent_modal: {
            layout: '#consent_layout#',                      // box,cloud,bar
            position: '#consent_position#',           // bottom,middle,top + left,right,center
            transition: '#consent_transition#'                 // zoom,slide
        },
        settings_modal: {
            layout: '#settings_layout#',                      // box,bar
            position: '#settings_position#',                     // right,left (available only if bar layout selected)
            transition: '#settings_transition#'                 // zoom,slide
        }
    },

    onFirstAction: function(){
        
    },

    onAccept: function (cookie) {
        
    },

    onChange: function (cookie, changed_preferences) {
        
    },

    languages: {
        '#current_lang#': {
            consent_modal: {
                title: cookie + ' #consent_title#',
                description: '#consent_description#',
                primary_btn: {
                    text: '#consent_primary_button#',
                    role: 'accept_all'              // 'accept_selected' or 'accept_all'
                },
                //SECSTARTsecondary_btn: {
                    text: '#consent_secondary_button#',
                    role: 'accept_necessary'        // 'settings' or 'accept_necessary'
                }//SECEND
            },
            settings_modal: {
                title: '🍪',
                save_settings_btn: '#settings_save_button#',
                accept_all_btn: '#settings_accept_all_button#',
                reject_all_btn: '#settings_reject_all_button#',
                close_btn_label: '#settings_close_button#',
                cookie_table_headers: [
                    {col1: '#cookie_table_header_1#'},
                    {col2: '#cookie_table_header_2#'},
                    {col3: '#cookie_table_header_3#'},
                    {col4: '#cookie_table_header_4#'},
                ],
                blocks: [
                    {#blocks#}
                ]
            }
        }
    }
});
