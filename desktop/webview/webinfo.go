package main

import "github.com/webview/webview"

func main() {
	debug := true
	w := webview.New(debug)
	defer w.Destroy()
	w.SetTitle("WebInfo")
	w.SetSize(800, 600, webview.HintNone)
	w.Navigate("http://localhost:9090")
	w.Run()
}