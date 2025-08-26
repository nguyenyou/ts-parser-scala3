package www

import com.raquo.laminar.api.L.*

case class App() {
    def apply(): HtmlElement = {
        div("Typescript Parser written in Scala 3")
    }
}