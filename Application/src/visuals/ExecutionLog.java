package visuals;

import javafx.scene.web.HTMLEditor;

import java.util.ArrayList;

public class ExecutionLog extends HTMLEditor {
    ArrayList<String> lines = new ArrayList<>();
    // https://www.w3schools.com/cssref/pr_dim_line-height.asp
    private final String HEADER =
            " <!DOCTYPE html><html>" +
                "<head style=\"" +
                    "p {line-height:80%;} " +
                    "body {background-color:black; color:white;}" +
                    //"font-family: \"Courier New\", Courier, monospace" +
                "\">" +
                    encapsulate("title", "Execution Log") +
                "</head>" +
            "<body>";
    private final String FOOTER = "</body></html>";

    public ExecutionLog() {
    }

    public void newLine(String line) {
        lines.add(line);
        StringBuilder sb = new StringBuilder();
        sb.append(HEADER);
        sb.append("<div contentEditable=\"false\">");   // https://www.w3schools.com/tags/att_global_contenteditable.asp
        sb.append("<p>");
        for (int i = 0; i < lines.size()-1; i++) {
            sb.append(lines.get(i));
            sb.append("<br>");
        }
        sb.append("</p>");
        sb.append("<h4 style=\"margin-top:-12px; background-color:#002309;\">");
        sb.append(lines.get(lines.size()-1));
        sb.append("</h4>");
        sb.append("</div>");
        sb.append(FOOTER);
        System.out.println(sb.toString());
        this.setHtmlText(sb.toString());
    }

    private static String encapsulate(String tag, String content) {
        return "<" + tag + ">" + content + "</" + tag + ">";
    }

}
