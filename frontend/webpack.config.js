const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const nodePath = process.env.NODE_PATH || "node_modules";

module.exports = {
  entry: "./index.js",
  output: {
    filename: "bundle.[contenthash].js",
    path: path.resolve(__dirname, "dist"),
    publicPath: "/",
    clean: true,
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: "elm-webpack-loader",
          options: {
            optimize: process.env.NODE_ENV === "production",
            debug: false,
          },
        },
      },
      {
        test: /\.s[ac]ss$/i,
        use: ["style-loader", "css-loader", "sass-loader"],
      },
    ],
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: "index.html",
      inject: "body",
    }),
  ],
  devServer: {
    static: {
      directory: path.join(__dirname, "dist"),
    },
    compress: false,
    port: 3000,
    hot: true,
    liveReload: true,
    historyApiFallback: true,
    proxy: [
      {
        context: ["/api", "/backend", "/docs"],
        target: process.env.PROXY_TARGET || "http://localhost:8080",
        secure: !process.env.PROXY_TARGET, // disable SSL for local dev
        changeOrigin: true,
      },
    ],
  },

  resolve: {
    modules: nodePath.split(":"),
  },

  resolveLoader: {
    modules: nodePath.split(":"),
  },
};
