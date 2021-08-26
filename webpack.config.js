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
        },
        {
            test: /\.(css)$/,
            use: ["style-loader", "css-loader"]
        },
        {
            test: /\.(png|jpe?g|gif)$/i,
            use: [
            {
                loader: 'file-loader',
            },
            ],
        }
    ]
    }
}