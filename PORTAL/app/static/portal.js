(() => {
  'use strict';

  const MAX_FILE_SIZE = 15 * 1024 * 1024;
  const REQUEST_PATTERN = /^SOL-[0-9]{4}-[0-9]{6}$/;
  const $ = id => document.getElementById(id);
  const params = new URLSearchParams(window.location.search);
  const requestCode = (params.get('solicitud') || '').trim().toUpperCase();
  const previewMode = params.get('vista') === 'previa';
  let sessionToken = sessionStorage.getItem(`documentos:${requestCode}`) || '';
  let requestData = null;
  let activeFormCode = '';
  let resendTimer = null;
  const selectedFiles = new Map();
  const signatureStrokes = [];
  let signatureContext = null;
  let activeSignatureStroke = null;

  const setStatus = (id, message, state = '') => {
    const element = $(id);
    element.textContent = message;
    if (state) element.dataset.state = state;
    else delete element.dataset.state;
  };

  const api = async (path, options = {}) => {
    const headers = new Headers(options.headers || {});
    if (sessionToken) headers.set('Authorization', `Bearer ${sessionToken}`);
    if (options.body && !(options.body instanceof FormData)) headers.set('Content-Type', 'application/json');
    const response = await fetch(`./api/${path}`, {...options, headers, credentials: 'same-origin'});
    const data = await response.json().catch(() => ({}));
    if (!response.ok) throw new Error(data.detail || 'No fue posible completar la operación.');
    return data;
  };

  const startResendTimer = seconds => {
    let remaining = seconds;
    const button = $('resend-otp');
    clearInterval(resendTimer);
    button.disabled = true;
    button.textContent = `Reenviar código (${remaining}s)`;
    resendTimer = window.setInterval(() => {
      remaining -= 1;
      button.textContent = remaining > 0 ? `Reenviar código (${remaining}s)` : 'Reenviar código';
      if (remaining <= 0) {
        clearInterval(resendTimer);
        button.disabled = false;
      }
    }, 1000);
  };

  const requestOtp = async () => {
    if (!REQUEST_PATTERN.test(requestCode)) return;
    $('request-otp').disabled = true;
    setStatus('access-status', 'Enviando código de verificación…');
    try {
      const result = await api('acceso/otp/solicitar', {
        method: 'POST', body: JSON.stringify({solicitud: requestCode})
      });
      $('otp-form').hidden = false;
      $('otp-code').focus();
      setStatus('access-status', result.message || 'Código enviado al correo registrado.', 'success');
      startResendTimer(result.retry_after_seconds || 60);
    } catch (error) {
      setStatus('access-status', error.message, 'error');
    } finally {
      $('request-otp').disabled = false;
    }
  };

  const verifyOtp = async event => {
    event.preventDefault();
    const code = $('otp-code').value.replace(/\D/g, '');
    if (code.length !== 6) {
      setStatus('access-status', 'Ingrese el código de seis dígitos.', 'error');
      return;
    }
    setStatus('access-status', 'Validando código…');
    try {
      const result = await api('acceso/otp/verificar', {
        method: 'POST', body: JSON.stringify({solicitud: requestCode, codigo: code})
      });
      sessionToken = result.session_token;
      sessionStorage.setItem(`documentos:${requestCode}`, sessionToken);
      await loadRequest();
    } catch (error) {
      setStatus('access-status', error.message, 'error');
    }
  };

  const formatDate = value => {
    if (!value) return '—';
    return new Intl.DateTimeFormat('es-CO', {
      timeZone: 'America/Bogota', day: '2-digit', month: '2-digit', year: 'numeric',
      hour: '2-digit', minute: '2-digit', second: '2-digit', hour12: false
    }).format(new Date(value));
  };

  const renderFormRequirements = requirements => {
    const list = $('form-list');
    list.textContent = '';
    requirements.forEach((requirement, index) => {
      if (requirement.tipo !== 'FORMULARIO' && !requirement.codigo.startsWith('FORM_')) return;
      const article = document.createElement('article');
      article.className = 'form-item';
      article.dataset.requirement = requirement.codigo;

      const number = document.createElement('div');
      number.className = 'step-number';
      number.setAttribute('aria-hidden', 'true');
      number.textContent = String(index + 1).padStart(2, '0');

      const copy = document.createElement('div');
      copy.className = 'form-copy';
      const titleLine = document.createElement('div');
      titleLine.className = 'form-title-line';
      const title = document.createElement('h3');
      title.textContent = requirement.nombre || requirement.codigo;
      const badge = document.createElement('span');
      badge.className = 'badge';
      titleLine.append(title, badge);
      const description = document.createElement('p');
      description.textContent = 'Revise la información solicitada y guarde el formulario.';
      copy.append(titleLine, description);

      const button = document.createElement('button');
      button.className = 'button button-primary';
      button.type = 'button';
      button.dataset.openForm = requirement.codigo;
      button.textContent = requirement.estado === 'COMPLETADO' ? 'Revisar formulario' : 'Diligenciar formulario';

      article.append(number, copy, button);
      list.append(article);
    });
  };

  const updateRequirement = requirement => {
    const card = document.querySelector(`[data-requirement="${requirement.codigo}"]`);
    if (!card) return;
    const badge = card.querySelector('.badge');
    const complete = requirement.estado === 'COMPLETADO';
    badge.textContent = complete ? 'Completo' : 'Pendiente';
    badge.dataset.state = complete ? 'complete' : 'pending';
  };

  const renderRequest = data => {
    requestData = data;
    $('request-code').textContent = data.codigo_solicitud;
    $('third-party-name').textContent = data.tercero.nombre_razon_social;
    $('third-party-id').textContent = `${data.tercero.tipo_identificacion} ${data.tercero.numero_identificacion}`;
    $('request-expiry').textContent = formatDate(data.vence_en);
    const requestedCodes = new Set(data.requisitos.map(item => item.codigo));
    renderFormRequirements(data.requisitos);
    document.querySelectorAll('[data-requirement]').forEach(card => {
      card.hidden = !requestedCodes.has(card.dataset.requirement);
    });
    data.requisitos.forEach(updateRequirement);
    const total = data.requisitos.length;
    const complete = data.requisitos.filter(item => item.estado === 'COMPLETADO').length;
    const percentage = total ? Math.round(100 * complete / total) : 0;
    $('progress-label').textContent = `${complete} de ${total} completos`;
    $('progress-bar').value = percentage;
    $('progress-bar').textContent = `${percentage}%`;
    $('submit-request').disabled = !total || complete !== total;
    $('access-panel').hidden = true;
    $('request-panel').hidden = false;
    window.requestAnimationFrame(resizeSignatureCanvas);
  };

  const loadRequest = async () => {
    try {
      const data = await api(`solicitudes/${encodeURIComponent(requestCode)}`);
      renderRequest(data);
    } catch (error) {
      sessionToken = '';
      sessionStorage.removeItem(`documentos:${requestCode}`);
      $('access-panel').hidden = false;
      $('request-panel').hidden = true;
      setStatus('access-status', error.message, 'error');
    }
  };

  const setSelectedFile = (code, file) => {
    const input = document.querySelector(`[data-file="${code}"]`);
    const label = document.querySelector(`[data-file-name="${code}"]`);
    const button = document.querySelector(`[data-upload="${code}"]`);
    const clear = document.querySelector(`[data-clear-file="${code}"]`);
    button.disabled = true;
    if (!file) {
      selectedFiles.delete(code);
      input.value = '';
      label.textContent = 'Ningún archivo seleccionado';
      clear.hidden = true;
      return;
    }
    if (file.type !== 'application/pdf' || !file.name.toLowerCase().endsWith('.pdf')) {
      input.value = '';
      label.textContent = 'Seleccione un archivo PDF válido.';
      clear.hidden = true;
      return;
    }
    if (file.size > MAX_FILE_SIZE) {
      input.value = '';
      label.textContent = 'El archivo supera el límite de 15 MB.';
      clear.hidden = true;
      return;
    }
    selectedFiles.set(code, file);
    label.textContent = file.name;
    clear.hidden = false;
    button.disabled = false;
  };

  const selectFile = event => {
    const input = event.currentTarget;
    setSelectedFile(input.dataset.file, input.files[0]);
  };

  const dropFile = event => {
    event.preventDefault();
    const dropzone = event.currentTarget;
    delete dropzone.dataset.dragging;
    setSelectedFile(dropzone.dataset.dropzone, event.dataTransfer.files[0]);
  };

  const uploadFile = async event => {
    const button = event.currentTarget;
    const code = button.dataset.upload;
    const file = selectedFiles.get(code);
    if (!file) return;
    if (previewMode) {
      setStatus('request-status', 'Vista previa: el archivo seleccionado no fue enviado ni almacenado.', 'success');
      return;
    }
    button.disabled = true;
    setStatus('request-status', `Adjuntando ${file.name}…`);
    const body = new FormData();
    body.append('archivo', file, file.name);
    try {
      await api(`solicitudes/${encodeURIComponent(requestCode)}/requisitos/${code}/archivo`, {
        method: 'POST', body
      });
      setStatus('request-status', 'Documento adjuntado correctamente.', 'success');
      await loadRequest();
    } catch (error) {
      button.disabled = false;
      setStatus('request-status', error.message, 'error');
    }
  };

  const drawSignatureStroke = (stroke, startIndex = 0) => {
    if (!signatureContext || !stroke.length) return;
    const canvas = $('signature-canvas');
    const width = canvas.clientWidth;
    const height = canvas.clientHeight;
    const first = stroke[startIndex];
    signatureContext.beginPath();
    signatureContext.moveTo(first.x * width, first.y * height);
    if (stroke.length - startIndex === 1) {
      signatureContext.lineTo(first.x * width + .01, first.y * height + .01);
    } else {
      stroke.slice(startIndex + 1).forEach(point => {
        signatureContext.lineTo(point.x * width, point.y * height);
      });
    }
    signatureContext.stroke();
  };

  const redrawSignature = () => {
    if (!signatureContext) return;
    const canvas = $('signature-canvas');
    signatureContext.clearRect(0, 0, canvas.clientWidth, canvas.clientHeight);
    signatureStrokes.forEach(stroke => drawSignatureStroke(stroke));
  };

  const resizeSignatureCanvas = () => {
    const canvas = $('signature-canvas');
    const rect = canvas.getBoundingClientRect();
    if (!rect.width || !rect.height) return;
    const ratio = Math.max(window.devicePixelRatio || 1, 1);
    canvas.width = Math.round(rect.width * ratio);
    canvas.height = Math.round(rect.height * ratio);
    signatureContext = canvas.getContext('2d');
    signatureContext.setTransform(ratio, 0, 0, ratio, 0, 0);
    signatureContext.lineWidth = 2;
    signatureContext.lineCap = 'round';
    signatureContext.lineJoin = 'round';
    signatureContext.strokeStyle = '#17202a';
    redrawSignature();
  };

  const signaturePoint = event => {
    const rect = $('signature-canvas').getBoundingClientRect();
    return {
      x: Math.min(Math.max((event.clientX - rect.left) / rect.width, 0), 1),
      y: Math.min(Math.max((event.clientY - rect.top) / rect.height, 0), 1)
    };
  };

  const beginSignature = event => {
    if (event.pointerType === 'mouse' && event.button !== 0) return;
    event.preventDefault();
    const canvas = $('signature-canvas');
    canvas.setPointerCapture(event.pointerId);
    activeSignatureStroke = [signaturePoint(event)];
    signatureStrokes.push(activeSignatureStroke);
    drawSignatureStroke(activeSignatureStroke);
    $('signature-placeholder').hidden = true;
  };

  const continueSignature = event => {
    if (!activeSignatureStroke) return;
    event.preventDefault();
    activeSignatureStroke.push(signaturePoint(event));
    drawSignatureStroke(activeSignatureStroke, Math.max(activeSignatureStroke.length - 2, 0));
  };

  const endSignature = event => {
    if (!activeSignatureStroke) return;
    event.preventDefault();
    const canvas = $('signature-canvas');
    if (canvas.hasPointerCapture(event.pointerId)) canvas.releasePointerCapture(event.pointerId);
    activeSignatureStroke = null;
    $('clear-signature').disabled = false;
    $('signature-status').textContent = 'Firma registrada';
    $('signature-status').dataset.state = 'complete';
  };

  const clearSignature = () => {
    signatureStrokes.length = 0;
    activeSignatureStroke = null;
    redrawSignature();
    $('signature-placeholder').hidden = false;
    $('clear-signature').disabled = true;
    $('signature-status').textContent = 'Firma pendiente';
    delete $('signature-status').dataset.state;
  };

  const initializeSignature = () => {
    const canvas = $('signature-canvas');
    canvas.addEventListener('pointerdown', beginSignature);
    canvas.addEventListener('pointermove', continueSignature);
    canvas.addEventListener('pointerup', endSignature);
    canvas.addEventListener('pointercancel', endSignature);
    $('clear-signature').addEventListener('click', clearSignature);
    if ('ResizeObserver' in window) {
      new ResizeObserver(resizeSignatureCanvas).observe(canvas);
    } else {
      window.addEventListener('resize', resizeSignatureCanvas);
    }
  };

  const previewForm = code => ({
    formulario: {
      codigo: code,
      nombre: 'Autorización para el tratamiento de datos personales',
      descripcion: [
        'Autorizo de manera previa, expresa e informada a Racafé & Cía. S.C.A. para recolectar, almacenar,',
        'usar, circular, actualizar y suprimir mis datos personales conforme a su política de tratamiento',
        'de información y a las finalidades indicadas a continuación.'
      ].join(' '),
      version: 1,
      campos: [
        {
          codigo: 'AUTORIZA_CREACION',
          etiqueta: 'Autoriza el tratamiento de la información para crear o actualizar el tercero.',
          tipo: 'CASILLA', obligatorio: true, opciones: '', ayuda: ''
        },
        {
          codigo: 'AUTORIZA_VALIDACION',
          etiqueta: 'Autoriza consultar fuentes públicas, listas de control y validaciones aplicables.',
          tipo: 'CASILLA', obligatorio: true, opciones: '', ayuda: ''
        },
        {
          codigo: 'AUTORIZA_COMUNICACIONES',
          etiqueta: 'Autoriza recibir comunicaciones relacionadas con la relación comercial.',
          tipo: 'CASILLA', obligatorio: false, opciones: '', ayuda: 'Esta autorización es opcional.'
        }
      ]
    },
    respuestas: null
  });

  const renderAuthorization = payload => {
    const form = payload.formulario;
    const saved = payload.respuestas && payload.respuestas.respuestas ? payload.respuestas.respuestas : {};
    activeFormCode = form.codigo;
    $('authorization-title').textContent = form.nombre;
    $('authorization-description').textContent = form.descripcion;
    $('authorization-name').textContent = requestData.tercero.nombre_razon_social;
    $('authorization-identification').textContent =
      `${requestData.tercero.tipo_identificacion} ${requestData.tercero.numero_identificacion}`;

    const container = $('authorization-fields');
    container.textContent = '';
    form.campos.forEach(field => {
      const wrapper = document.createElement('label');
      wrapper.className = `authorization-field authorization-field-${field.tipo.toLowerCase()}`;
      const id = `form-field-${field.codigo}`;
      let control;

      if (field.tipo === 'CASILLA') {
        control = document.createElement('input');
        control.type = 'checkbox';
        control.checked = saved[field.codigo] === true;
        const text = document.createElement('span');
        text.textContent = field.etiqueta;
        wrapper.append(control, text);
      } else {
        const text = document.createElement('span');
        text.textContent = field.etiqueta;
        wrapper.append(text);
        if (field.tipo === 'TEXTO_LARGO') {
          control = document.createElement('textarea');
          control.rows = 4;
        } else if (field.tipo === 'SELECCION') {
          control = document.createElement('select');
          const empty = document.createElement('option');
          empty.value = '';
          empty.textContent = 'Seleccione una opción';
          control.append(empty);
          String(field.opciones || '').split('|').map(value => value.trim()).filter(Boolean).forEach(value => {
            const option = document.createElement('option');
            option.value = value;
            option.textContent = value;
            control.append(option);
          });
        } else {
          control = document.createElement('input');
          control.type = 'text';
        }
        control.value = saved[field.codigo] || '';
        wrapper.append(control);
      }

      control.id = id;
      control.dataset.formField = field.codigo;
      control.dataset.formType = field.tipo;
      control.required = field.obligatorio === true;
      if (field.ayuda) {
        const help = document.createElement('small');
        help.textContent = field.ayuda;
        wrapper.append(help);
      }
      container.append(wrapper);
    });
  };

  const openAuthorization = async formCode => {
    try {
      const payload = previewMode
        ? previewForm(formCode)
        : await api(`solicitudes/${encodeURIComponent(requestCode)}/formularios/${encodeURIComponent(formCode)}`);
      renderAuthorization(payload);
      $('authorization-dialog').showModal();
    } catch (error) {
      setStatus('request-status', error.message, 'error');
    }
  };

  const formAnswers = () => {
    const answers = {};
    document.querySelectorAll('[data-form-field]').forEach(control => {
      answers[control.dataset.formField] =
        control.dataset.formType === 'CASILLA' ? control.checked : control.value.trim();
    });
    return answers;
  };

  const submitRequest = async () => {
    if (previewMode) {
      setStatus('request-status', 'Vista previa: la solicitud no fue enviada.', 'success');
      return;
    }
    $('submit-request').disabled = true;
    setStatus('request-status', 'Enviando documentación para revisión…');
    try {
      await api(`solicitudes/${encodeURIComponent(requestCode)}/finalizar`, {method: 'POST'});
      setStatus('request-status', 'La documentación fue enviada correctamente para revisión.', 'success');
      await loadRequest();
    } catch (error) {
      $('submit-request').disabled = false;
      setStatus('request-status', error.message, 'error');
    }
  };

  const initialize = () => {
    $('request-otp').addEventListener('click', requestOtp);
    $('resend-otp').addEventListener('click', requestOtp);
    $('otp-form').addEventListener('submit', verifyOtp);
    document.querySelectorAll('[data-file]').forEach(input => input.addEventListener('change', selectFile));
    document.querySelectorAll('[data-dropzone]').forEach(dropzone => {
      dropzone.addEventListener('dragenter', event => {
        event.preventDefault();
        dropzone.dataset.dragging = 'true';
      });
      dropzone.addEventListener('dragover', event => event.preventDefault());
      dropzone.addEventListener('dragleave', () => delete dropzone.dataset.dragging);
      dropzone.addEventListener('drop', dropFile);
    });
    document.querySelectorAll('[data-clear-file]').forEach(button => {
      button.addEventListener('click', () => setSelectedFile(button.dataset.clearFile, null));
    });
    document.querySelectorAll('[data-upload]').forEach(button => button.addEventListener('click', uploadFile));
    $('form-list').addEventListener('click', event => {
      const button = event.target.closest('[data-open-form]');
      if (button) openAuthorization(button.dataset.openForm);
    });
    $('submit-request').addEventListener('click', submitRequest);
    $('close-authorization').addEventListener('click', () => $('authorization-dialog').close());
    $('cancel-authorization').addEventListener('click', () => $('authorization-dialog').close());
    $('authorization-form').addEventListener('submit', async event => {
      event.preventDefault();
      if (!$('authorization-form').reportValidity()) return;
      if (previewMode) {
        $('authorization-dialog').close();
        setStatus('request-status', 'Vista previa: el formulario fue diligenciado, pero no se guardó.', 'success');
        return;
      }
      setStatus('request-status', 'Guardando formulario…');
      try {
        await api(
          `solicitudes/${encodeURIComponent(requestCode)}/formularios/${encodeURIComponent(activeFormCode)}`,
          {method: 'POST', body: JSON.stringify({respuestas: formAnswers()})}
        );
        $('authorization-dialog').close();
        setStatus('request-status', 'Formulario guardado correctamente.', 'success');
        await loadRequest();
      } catch (error) {
        setStatus('request-status', error.message, 'error');
      }
    });
    initializeSignature();

    if (previewMode) {
      document.body.classList.add('preview-mode');
      $('preview-notice').hidden = false;
      renderRequest({
        codigo_solicitud: 'SOL-2026-000000',
        vence_en: new Date(Date.now() + 7 * 24 * 60 * 60 * 1000).toISOString(),
        tercero: {
          nombre_razon_social: 'Empresa de ejemplo S.A.S.',
          tipo_identificacion: 'NIT',
          numero_identificacion: '900123456'
        },
        requisitos: [
          {codigo: 'DOC_RUT', tipo: 'DOCUMENTO', nombre: 'RUT', estado: 'COMPLETADO'},
          {
            codigo: 'DOC_CERL', tipo: 'DOCUMENTO',
            nombre: 'Certificado de existencia y representación legal', estado: 'PENDIENTE'
          },
          {
            codigo: 'FORM_HABEAS', tipo: 'FORMULARIO',
            nombre: 'Autorización de tratamiento de datos personales', estado: 'PENDIENTE'
          }
        ]
      });
      return;
    }

    if (!REQUEST_PATTERN.test(requestCode)) {
      $('request-otp').disabled = true;
      setStatus('access-status', 'El enlace no contiene una solicitud válida.', 'error');
      return;
    }
    $('request-reference').textContent = `Solicitud ${requestCode}`;
    if (sessionToken) loadRequest();
  };

  initialize();
})();
