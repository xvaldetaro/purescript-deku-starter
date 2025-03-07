const path = require("path");
module.exports = {
	mode: "production",
	entry: "./src/index.js",
	output: {
		path: path.resolve(__dirname, "docs"),
		filename: "bundle.js",
	},
	module: {
		rules: [
			{
				test: /\.js$/i,
				include: path.resolve(__dirname, "src"),
				use: {
					loader: "babel-loader",
					options: {
						presets: ["@babel/preset-env"],
					},
				},
			},
		],
	},
	devServer: {
		contentBase: path.resolve(__dirname, "docs"),
		watchContentBase: true,
	},
};
