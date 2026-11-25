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
      Router.route Login ~path:"/login" ~page:login ~search:{ failed = optional bool };
      Router.post Login_post ~path:"/ui/login" ~handler:Handlers.login;
      Router.post Logout ~path:"/ui/logout" ~handler:Handlers.logout;
      Router.post Elevate_post ~path:"/ui/elevate" ~handler:Handlers.elevate;
      Router.post Theme ~path:"/ui/theme" ~handler:Handlers.theme;
      Router.post Password ~path:"/ui/password" ~handler:Handlers.password;
      Router.post App_create ~path:"/ui/applications" ~handler:Handlers.app_create;
      Router.post App_update ~path:"/ui/applications/:id<int>" ~handler:Handlers.app_update;
      Router.post App_rotate ~path:"/ui/applications/:id<int>/rotate" ~handler:Handlers.app_rotate;
      Router.post App_delete ~path:"/ui/applications/:id<int>/delete" ~handler:Handlers.app_delete;
      Router.post Client_create ~path:"/ui/clients" ~handler:Handlers.client_create;
      Router.post Client_update ~path:"/ui/clients/:id<int>" ~handler:Handlers.client_update;
      Router.post Client_delete ~path:"/ui/clients/:id<int>/delete" ~handler:Handlers.client_delete;
      Router.post User_create ~path:"/ui/users" ~handler:Handlers.user_create;
      Router.post User_update ~path:"/ui/users/:id<int>" ~handler:Handlers.user_update;
      Router.post User_delete ~path:"/ui/users/:id<int>/delete" ~handler:Handlers.user_delete;
      Router.group ~path:"/" ~loader:Loaders.session ~layout:shell ~search:{ alert = optional string }
        [
          Router.route Messages ~path:"/" ~page:messages;
          Router.route App_messages ~path:"/messages/:app<int>" ~page:app_messages;
          Router.route Applications ~path:"/applications" ~page:applications ~search:{ reveal = optional string };
          Router.route Clients ~path:"/clients" ~page:Pages.clients ~loader:Loaders.clients ~search:{ reveal = optional string };
          Router.route Users ~path:"/users" ~page:Pages.users ~loader:Loaders.users;
          Router.route Settings ~path:"/settings" ~page:settings;
          Router.route Elevate ~path:"/elevate" ~page:elevate ~search:{ next = optional string };
        ];
    ]
