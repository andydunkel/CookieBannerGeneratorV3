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
