module Toolbox.Server.Startup

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Giraffe

type Startup(cfg:IConfiguration, env:IWebHostEnvironment) =
    member _.ConfigureServices (services:IServiceCollection) =
        services
            .AddApplicationInsightsTelemetry(fun options ->
                options.ConnectionString <- cfg.["APPINSIGHTS_CONNECTIONSTRING"])
            .AddGiraffe() |> ignore

    member _.Configure(app:IApplicationBuilder) =
        app
            .UseStaticFiles()
            .UseGiraffe WebApp.webApp