# Elm eCommerce

This is a small demo of an eCommerce application written in Elm as demonstration material for an Elm workshop.

* The app demo: https://elm-ecommerce.surge.sh 
* The app + The dashboard demo: https://elm-ecommerce-dashboard.surge.sh
* The app demo in Ellie: https://ellie-app.com/cZbNgHS6hK8a1
* The code: https://github.com/lucamug/elm-ecommerce

![picture](docs/elm-ecommerce-logo.png)

## Prerequisites

Install [Node.js](https://nodejs.org/).

All other commands should work without installation as they are using `npx`. In case there are some issues you can install the required application executing:

### Optional installations for the development

```
npm i -g elm
npm i -g elm-go
```

### Optional installations for building the optimized version to be released

```
npm i -g terser
npm i -g surge
```


## To set up the development environment

```
git clone https://github.com/lucamug/elm-ecommerce.git
cd elm-ecommerce
cmd/start
```

Then access http://localhost:8000/

Edit `src/Main.elm` and the browser will update automatically.

## To start the Dashboard

```
cmd/startDashboard
```

Then access http://localhost:8001/

Edit `src/Dashboard.elm` and the browser will update automatically.

## To build an optimized version

```
cmd/build
```

This will create two folders, `build/normal` and `build/dashboard` that can be used to publish the application.

You can see these two versions of the application here:

* https://elm-ecommerce.surge.sh
* https://elm-ecommerce-dashboard.surge.sh


# **❤️😃 HAPPY CODING! 😃❤️**