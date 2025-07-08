function getQueryParam(param) {
    const urlParams = new URLSearchParams(window.location.search);
    return urlParams.get(param);
}

fetch('/tags.json')
    .then(response => response.json())
    .then(data => {
        const tag = getQueryParam("tag");
        const tagList = document.getElementById("tag-list");
        const postList = document.getElementById("post-list");
        const tagTitle = document.getElementById("tag-title");

        tagList.innerHTML = "";
        postList.innerHTML = "";

        // Show all tags as links
        Object.keys(data).sort().forEach(tagName => {
            const link = document.createElement("a");
            link.href = `?tag=${encodeURIComponent(tagName)}`;
            link.textContent = tagName;
            link.style.marginRight = "10px";
            tagList.appendChild(link);
        });

        if (!tag || !data[tag]) {
            return; // No tag selected, done
        }

        tagTitle.style.display = "block";
        tagTitle.textContent = `posts tagged with "${tag}"`;

        data[tag].forEach(post => {
            const div = document.createElement("div");
            div.innerHTML = `<a href="${post.url}">${post.title}</a>`;
            postList.appendChild(div);
        });
    });