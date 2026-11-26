open Pages

let security =
  [
    ("x-content-type-options", "nosniff");
    ("x-frame-options", "DENY");
    ("referrer-policy", "same-origin");
  ]

let routes =
  Router.make ~not_found ~error:error_page ~headers:security ~cache:no_store
    [
      Router.route Login ~path:"/login" ~page:login ~search:{ failed = optional bool; registered = optional bool };
      Router.route Register ~path:"/register" ~page:register ~search:{ alert = optional string };
      Router.post Register_post ~path:"/ui/register" ~handler:Handlers.register_post;
      Router.post Login_post ~path:"/ui/login" ~handler:Handlers.login;
      Router.post Logout ~path:"/ui/logout" ~handler:Handlers.logout;
      Router.post Theme ~path:"/ui/theme" ~handler:Handlers.theme;
      Router.post Message_delete ~path:"/ui/messages/:id<int>/delete" ~handler:Handlers.message_delete;
      Router.post App_move ~path:"/ui/applications/:id<int>/move" ~handler:Handlers.app_move;
      Router.post App_image ~path:"/ui/applications/:id<int>/image" ~handler:Handlers.app_image;
      Router.group ~path:"/" ~loader:Loaders.session ~layout:shell ~search:{ alert = optional string }
        [
          Router.route Messages ~path:"/" ~page:messages ~search:{ pages = default int 1 };
          Router.route Messages_remove ~path:"/messages/delete" ~page:messages_remove ?overlay:messages_remove_dialog;
          Router.route Message_remove ~path:"/messages/delete/:id<int>" ~page:message_remove ~loader:Loaders.message
            ~search:{ back = optional string };
          Router.route App_messages ~path:"/messages/:app<int>" ~page:app_messages ~search:{ pages = default int 1 };
          Router.route App_messages_remove ~path:"/messages/:app<int>/delete" ~page:app_messages_remove
            ?overlay:app_messages_remove_dialog;
          Router.route App_push ~path:"/messages/:app<int>/push" ~page:app_push ?overlay:app_push_dialog;
          Router.route Applications ~path:"/applications" ~page:applications ~search:{ reveal = optional string };
          Router.route App_new ~path:"/applications/new" ~page:app_new ?overlay:app_new_dialog;
          Router.route App_edit ~path:"/applications/:id<int>/edit" ~page:app_edit ?overlay:app_edit_dialog;
          Router.route App_remove ~path:"/applications/:id<int>/delete" ~page:app_remove ?overlay:app_remove_dialog;
          Router.route App_token ~path:"/applications/:id<int>/token" ~page:app_token ?overlay:app_token_dialog;
          Router.route App_image_remove ~path:"/applications/:id<int>/image/delete" ~page:app_image_remove
            ?overlay:app_image_remove_dialog;
          Router.route Clients ~path:"/clients" ~page:Pages.clients ~loader:Loaders.clients ~search:{ reveal = optional string };
          Router.route Client_new ~path:"/clients/new" ~page:client_new ?overlay:client_new_dialog;
          Router.route Client_edit ~path:"/clients/:id<int>/edit" ~page:client_edit ?overlay:client_edit_dialog
            ~loader:Loaders.client;
          Router.route Client_elevate ~path:"/clients/:id<int>/elevate" ~page:client_elevate ?overlay:client_elevate_dialog
            ~loader:Loaders.client;
          Router.route Client_remove ~path:"/clients/:id<int>/delete" ~page:client_remove ?overlay:client_remove_dialog
            ~loader:Loaders.client;
          Router.route Users ~path:"/users" ~page:Pages.users ~loader:Loaders.users;
          Router.route User_new ~path:"/users/new" ~page:user_new ?overlay:user_new_dialog;
          Router.route User_edit ~path:"/users/:id<int>/edit" ~page:user_edit ?overlay:user_edit_dialog ~loader:Loaders.user;
          Router.route User_remove ~path:"/users/:id<int>/delete" ~page:user_remove ?overlay:user_remove_dialog
            ~loader:Loaders.user;
          Router.route Settings ~path:"/settings" ~page:settings;
        ];
    ]
