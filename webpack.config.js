const path = require("path")

module.exports = {
    mode: "none",
    entry: "./src/App.fsproj",
    devServer: {
        contentBase: path.join(__dirname, "./dist"),
        port: 8080, // not really necessary - defaults to 8080
    },
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        }]
    }
}