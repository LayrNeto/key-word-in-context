document.getElementById("kwicForm").addEventListener("submit", async function (event) {
    event.preventDefault();

    const fileInput = document.getElementById("fileInput").files[0];
    const stopWordsInput = document.getElementById("stopWordsInput").files[0]; 

    if (!fileInput) {
        alert("Please select a file to process.");
        return;
    }

    const formData = new FormData();
    formData.append("file", fileInput);

    if (stopWordsInput) {
        formData.append("stopWords", stopWordsInput);
    }

    try {
        const response = await fetch("/process", {
            method: "POST",
            body: formData,
        });

        if (!response.ok) {
            console.error("Server response error:", response.statusText);
            alert("Error processing the file. Check the format and try again.");
            return;
        }

        const result = await response.text();

        const parsedResult = JSON.parse(result); 
        const resultsArray = parsedResult.results;
        const outputDiv = document.getElementById("output");

        outputDiv.innerHTML = resultsArray.map(line => line.trim()).join('<br>');

    } catch (error) {
        console.error("Error sending the file:", error);
        alert("An error occurred while processing the file. Please try again later.");
    }
});
