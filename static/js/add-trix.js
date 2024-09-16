const editor = document.createElement("trix-editor");
editor.setAttribute("input", "mailing-content");
editor.setAttribute(
  "style",
  "overflow-y: auto; height: 400px; max-height: 600px"
);
const wrapper = document.getElementById("trix-wrapper");
wrapper.appendChild(editor);

const BASE = "/images";
const MAILING_ID = window.location.pathname.substring(
  window.location.pathname.lastIndexOf("/") + 1
);

function handleMSubmit(event) {
  if (event.submitter.innerHTML.toLowerCase().includes("send mailing")) {
    if (!confirm("Send to all contacts?")) {
      event.preventDefault();
    }
    return;
  }
}

addEventListener("trix-attachment-add", function (event) {
  if (window.location.pathname.endsWith("mailing")) {
    alert("Save draft first to add images");
    event.attachment.remove();
    return;
  }
  if (event.attachment.file) {
    uploadFileAttachment(event.attachment);
  }
});

addEventListener("trix-attachment-remove", function (event) {
  if (event.attachment) {
    const url = event.attachment.attachment.attributes.values?.url;
    if (!url) return;
    const filename = url.substring(url.lastIndexOf("/") + 1);
    fetch(BASE + "/" + MAILING_ID + "/" + encodeURI(filename), {
      method: "DELETE",
      credentials: "include",
    }).catch(console.error);
  }
});

function uploadFileAttachment(attachment) {
  uploadFile(attachment.file, setProgress, setAttributes, removeAttachment);

  function setProgress(progress) {
    attachment.setUploadProgress(progress);
  }

  function setAttributes(attributes) {
    console.log("setting attributes here");
    console.log({ attributes });
    attachment.setAttributes(attributes);
  }

  function removeAttachment() {
    attachment.remove();
  }
}

function uploadFile(file, progressCallback, successCallback, failureCallback) {
  var key = createStorageKey(file);
  var formData = createFormData(key, file);
  var xhr = new XMLHttpRequest();
  xhr.open("POST", `${BASE}/${MAILING_ID}`, true);

  xhr.upload.addEventListener("progress", function (event) {
    var progress = (event.loaded / event.total) * 100;
    progressCallback(progress);
  });

  xhr.addEventListener("load", function (event) {
    console.log({ xhr });
    if (xhr.status == 200) {
      console.log("got it");
      const resp = JSON.parse(xhr.response);
      var attributes = {
        url: resp.url,
        href: resp.url,
      };
      successCallback(attributes);
    } else {
      console.log(xhr);
      failureCallback();
      alert("Error uploading file");
    }
  });

  xhr.addEventListener("error", function (event) {
    console.error(event);
  });

  xhr.send(formData);
}

function createStorageKey(file) {
  var date = new Date();
  var day = date.toISOString().slice(0, 10);
  var name = date.getTime() + "-" + file.name;
  return ["tmp", day, name].join("/");
}

function createFormData(key, file) {
  var data = new FormData();
  data.append("key", key);
  data.append("Content-Type", file.type);
  data.append("file", file);
  return data;
}
