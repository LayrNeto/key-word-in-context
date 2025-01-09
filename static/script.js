document.getElementById("kwicForm").addEventListener("submit", async function (event) {
    event.preventDefault();

    const inputText = document.getElementById("inputText").value;

    try {
        const response = await fetch("/process", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({ input: inputText }),
        });

        if (!response.ok) {
            console.error("Erro na resposta do servidor:", response.statusText);
            return;
        }

        const result = await response.json();

        const outputDiv = document.getElementById("output");
        outputDiv.innerHTML = "<ul>" +
            result.map(item => `<li>${item}</li>`).join("") +
            "</ul>";
    } catch (error) {
        console.error("Erro ao processar o texto:", error);
    }
});