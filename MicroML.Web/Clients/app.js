document.getElementById('parseButton').addEventListener('click', async () => {
    const code = document.getElementById('codeInput').value;
    if (!code.trim()) {
        alert('Please enter some MicroML code to parse.');
        return;
    }

    try {
        const response = await fetch('/parse', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify(code)
        });

        if (!response.ok) {
            throw new Error(`Error: ${response.status}`);
        }

        const ast = await response.json();
        visualizeAST(ast);
    } catch (error) {
        console.error('Error:', error);
        document.getElementById('astVisualization').innerHTML =
            `<div class="error">Error parsing code: ${error.message}</div>`;
    }
});

function visualizeAST(node) {
    const container = document.getElementById('astVisualization');
    container.innerHTML = '';
    container.appendChild(createASTNodeElement(node));
}

function createASTNodeElement(node) {
    const element = document.createElement('div');
    element.className = 'node';

    const typeSpan = document.createElement('span');
    typeSpan.className = 'node-type';
    typeSpan.textContent = node.Type;
    element.appendChild(typeSpan);

    if (node.Value) {
        const valueSpan = document.createElement('span');
        valueSpan.className = 'node-value';
        valueSpan.textContent = node.Value;
        element.appendChild(valueSpan);
    }

    if (node.Children && node.Children.length > 0) {
        const childrenContainer = document.createElement('div');
        childrenContainer.className = 'children';

        node.Children.forEach(child => {
            const childContainer = document.createElement('div');
            childContainer.className = 'child';
            childContainer.appendChild(createASTNodeElement(child));
            childrenContainer.appendChild(childContainer);
        });

        element.appendChild(childrenContainer);
    }

    return element;
}
